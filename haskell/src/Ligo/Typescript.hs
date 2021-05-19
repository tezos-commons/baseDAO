-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-|

Thie module implements the functionality to generate Typescript types
from the parameter annotations generated using functions from `morley`
package.

-}

module Ligo.Typescript
  ( generateTs
  ) where

import Universum

import Data.Char (toUpper)
import qualified Data.Map as Map
import qualified Data.Text as T
import Fmt (Buildable(build), blockListF', (+|), (|+))
import qualified Fmt
import System.FilePath

import Lorentz.Entrypoints
import Michelson.Typed.Annotation
import Michelson.Typed.Entrypoints (pnNotes)
import qualified Michelson.Untyped as U
import Michelson.Untyped.Annotation

data AnnotatedField = AnnotatedField
  { afNote :: FieldAnn
  , afType :: U.Ty
  } deriving stock Show

-- An entrypoint extracted from parameter with entrypoint name
-- and the type of the parameter it wraps.
data Entrypoint = Entrypoint
  { epName :: Text
  , epType :: U.Ty
  } deriving stock (Show, Generic)

toType :: U.T -> U.Ty
toType t = U.Ty t noAnn

nToType :: Notes t -> U.Ty
nToType (NTString _) = toType U.TString
nToType (NTInt _) = toType U.TInt
nToType (NTNat _) = toType U.TNat
nToType (NTUnit _) = toType U.TUnit
nToType (NTSignature _) = toType U.TSignature
nToType (NTOption _ a) = toType (U.TOption (nToType a))
nToType (NTOr _ n1 n2 f1 f2) = toType $ U.TOr n1 n2 (nToType f1) (nToType f2)
nToType (NTPair _ n1 n2 va1 va2 f1 f2) = toType $ U.TPair n1 n2 va1 va2 (nToType f1) (nToType f2)
nToType (NTKey _) = toType $ U.TKey
nToType (NTChainId _) = toType $ U.TChainId
nToType (NTList _ a) = toType $ U.TList (nToType a)
nToType (NTSet _ a) = toType $ U.TSet (nToType a)
nToType (NTOperation _) = toType $ U.TOperation
nToType (NTMap _ f1 f2) = toType $ U.TMap (nToType f1) (nToType f2)
nToType (NTBigMap _ f1 f2) = toType $ U.TBigMap (nToType f1) (nToType f2)
nToType (NTContract _ a) = toType $ U.TContract (nToType a)
nToType (NTLambda _ f1 f2) = toType $ U.TLambda (nToType f1) (nToType f2)
nToType (NTBytes _) = toType U.TBytes
nToType (NTMutez _) = toType U.TMutez
nToType (NTKeyHash _) = toType U.TKeyHash
nToType (NTTimestamp _) = toType U.TTimestamp
nToType (NTAddress _) = toType U.TAddress
nToType (NTBool _) = toType U.TBool
nToType (NTBls12381Fr _) = toType U.TBls12381Fr
nToType (NTBls12381G1 _) = toType U.TBls12381G1
nToType (NTBls12381G2 _) = toType U.TBls12381G2
nToType (NTNever _) = toType U.TNever
nToType (NTTicket _ a) = toType $ U.TTicket (nToType a)

-- looks up the entrypoints in the 'Notes t' for the whole parameter.
lookupEntryPointsAndTypes :: Notes t -> [Entrypoint]
lookupEntryPointsAndTypes n =
    catMaybes $
      mkEntrypoint <$> (Map.assocs $ U.mkEntrypointsMap (U.ParameterType (nToType n) noAnn))
  where
    mkEntrypoint (a, b) = let
      epTxt = U.unEpName a
      in case (epTxt == "" ||  (isJust $ find (== epTxt) ignoreEntypoints)) of
        True -> Nothing
        _ -> Just $ Entrypoint (U.unEpName a) b

-- Some entrypoints we don't want to call directly.
ignoreEntypoints :: [Text]
ignoreEntypoints =
  [ "running"
  , "xtzForbidden"
  , "xtzAllowed"
  , "call_FA2"
  , "migratable"
  ]

type Typename = Text
type Fieldname = Text

-- Represents a Typescript declaration. For now only contains one that
-- defines a Type.
data TsDecl
  = TsType Typename TsTypeDef
  deriving stock Generic

-- Represents an instance of a Typescript type.
data TsType
  = TsNumber
  | TsString
  | TsBool
  | TsUnit
  | TsOption TsType
  | TsList TsType
  | TsMap TsType TsType
  | TsBigMap TsType TsType
  | TsCustom Typename
  | TsStrLiteral Text
  | TsVoid
  deriving stock Generic

-- Implements the conversion of Type to its source code representation.
instance Buildable TsType where
  build = \case
    TsNumber -> "number"
    TsString -> "string"
    TsBool -> "boolean"
    TsUnit -> "{}"
    TsOption t_ -> "null | " +| (build t_)
    TsList t_ -> "Array<" +| t_ |+ ">"
    TsMap k v -> "Map<" +| k |+ "," +| v |+ ">"
    TsBigMap k v -> "Map<" +| k |+ "," +| v |+ ">"
    TsCustom tn -> build tn
    TsStrLiteral tn -> build $ "\""  <> tn <> "\""
    TsVoid -> "void"

-- Represents a Typescript module with just enough stuff we are interested.
data TsModule = TsModule
  { tsmEpName :: Text
  , tsmImports :: [Text]
  , tsmDecls :: [TsDecl]
  , tsmExports :: [Typename]
  } deriving stock Generic

-- Represents the right side of a Type definition in Typescript.
data InterfaceField = InterfaceField
  { ifFieldName :: Fieldname
  , ifFieldType :: TsType
  , ifIsOptional :: Bool
  }

data TsTypeDef
  = TsUnion [TsType]
  | TsInterface [InterfaceField]
  | TsTuple [TsType]
  | TsAlias TsType
  deriving stock Generic

-- Implements the conversion to source code.
instance Buildable TsTypeDef where
  build (TsUnion xs) = Fmt.indentF 2 $ (Fmt.blockListF' "|" build xs)
  build (TsTuple fs) =  Fmt.unwordsF ["[", T.intercalate ", " (Fmt.pretty <$> fs), "];"]
  build (TsInterface fns) =  Fmt.unlinesF ["{", Fmt.indentF 2 $ Fmt.unlinesF $ mkPair <$> fns, "};"]
    where
      mkPair :: InterfaceField -> Text
      mkPair (InterfaceField fn tn isOptional) =
        case isOptional of
          True -> fn <> "?: " <> (Fmt.pretty tn) <> ";"
          False -> fn <> ": " <> (Fmt.pretty tn) <> ";"
  build (TsAlias t) = build t <> ";"

-- Implements the conversion to source code.
instance Buildable TsDecl where
  build (TsType typename tdef) = case tdef of
    TsUnion _ -> (Fmt.unwordsF ["export type", build typename, "=\n"]) <> (build tdef)
    TsTuple _ -> Fmt.unwordsF ["export type", build typename, "=", build tdef]
    TsInterface _ -> Fmt.unwordsF ["export interface", build typename, build tdef]
    TsAlias _ -> Fmt.unwordsF ["export type", build typename, "=", build tdef]

-- Write down Typescript modules to represents types for the contract parameter 'cp'
generateTs :: forall cp m. (MonadIO m, ParameterDeclaresEntrypoints cp) => FilePath -> m FilePath
generateTs fp = do
  let epModules = generateEpModule (pnNotes $ parameterEntrypointsToNotes @cp)
  mapM_ (writeModule fp) epModules
  let imports = (\ep -> "import {" <> (tsmEpName ep) <> "}"
        <> " from  './" <> (tsmEpName ep) <> "';") <$> epModules
  let parameterName = "Parameter"
  let parameterModule = TsModule
        parameterName (toText <$> imports)
          [TsType parameterName
            (TsUnion $ (\x -> let n = tsmEpName x in TsCustom n) <$> epModules)] [parameterName]
  writeModule fp parameterModule

-- Convert a 'Notes t' structure to a list of modules containing types
-- that wraps its various entrypoints. Each entrypoint is wrapped into
-- its own modules.
generateEpModule :: Notes t -> [TsModule]
generateEpModule notes = let
  eps = lookupEntryPointsAndTypes notes
  mkModule Entrypoint {..} = let
    capsdEp = ucFirst epName
    in TsModule capsdEp ["import {Lambda} from '../common';"] (mkTypesFor capsdEp epType) [capsdEp]
  in mkModule <$> eps

ucFirst :: Text -> Text
ucFirst x = T.cons (toUpper $ T.head x) (T.tail x)

-- Given a parent directory and a TsModule, write down the module as a
-- Typescript source file in the parent directory.
writeModule :: MonadIO m => FilePath -> TsModule -> m FilePath
writeModule fp (TsModule name imports decls _) = do
  let filename = (toString name) <.> "ts"
  let declLines = build <$> decls
  let importsLines = build <$> imports
  --let exportLines = (\x -> Fmt.unwordsF ["export", "{", x, "};"]) <$> exports
  writeFile (fp </> filename) (Fmt.pretty $ Fmt.unlinesF (importsLines <> declLines))
  pure filename

-- Given a typename, and a type, generates Typescript declarations for this
-- type and all of its fields. If there is at least one named field in the type, it is
-- converted into an interface, otherwise into a TypeScript tuple. 'Or' structures are
-- converted into Union types after flattening their structure. The rest of the types are
-- converted into the corresponding TypeScript types after creating type aliases for
-- their inner fields.
{-# ANN mkTypesFor ("HLint: ignore Reduce duplication" :: Text) #-}
mkTypesFor :: Typename -> U.Ty -> [TsDecl]
mkTypesFor typename epType = case U.unwrapT epType of
  U.TPair n1 n2 _ _ f1 f2 -> let
    fields1 = flattenPairs $ AnnotatedField n1 f1
    fields2 = flattenPairs $ AnnotatedField n2 f2
    allFields = fields1 <> fields2
    in case hasAtLeastOneNamedField allFields of
      False -> let
        allFieldTypes = snd <$> allFields
        decls = mkTypes_ <$> (zip allFieldTypes [0..])
        fieldTypeNames = fst <$> decls
        fieldDefinitions = snd <$> decls
        thisType = TsType typename (TsTuple fieldTypeNames)
        in thisType : (concat fieldDefinitions)
      True -> let
        decls = (mkTypesForField False) <$> (indexEmptyFields allFields)
        recordFields = fst <$> decls
        thisType = TsType typename (TsInterface recordFields)
        thisDecls = snd <$> decls
        in thisType : (concat thisDecls)

  U.TOr n1 n2 f1 f2 -> let
    fields1 = flattenOrs $ AnnotatedField n1 f1
    fields2 = flattenOrs $ AnnotatedField n2 f2

    decls = (mkTypesForField True) <$> (indexEmptyFields $ fields1 <> fields2)
    typeOptions = (ifFieldType . fst) <$> decls
      -- Throws away information about if the field is optional. Need to check if it is relevant in 'Or' structures
    thisType = TsType typename (TsUnion typeOptions)
    thisDecls = snd <$> decls
    in thisType : (concat thisDecls)

  U.TList a -> let
    elementTypeName = typename <> "Item"
    (inner, elementTypes) = mkTypesExcludingPrimitives elementTypeName a
    thisType = TsType typename (TsAlias $ TsList inner)
    in thisType : elementTypes

  U.TSet a -> let
    elementTypeName = typename <> "Item"
    (inner, elementTypes) = mkTypesExcludingPrimitives elementTypeName a
    thisType = TsType typename (TsAlias $ TsList inner)
    in thisType : elementTypes

  U.TOption a -> let
    elementTypeName = typename <> "Item"
    (inner, elementTypes) = mkTypesExcludingPrimitives elementTypeName a
    thisType = TsType typename (TsAlias $ TsOption inner)
    in thisType : elementTypes

  U.TMap k v -> let
    (kTypename, kDecls) = let
      kTypename_ = typename <> "Key"
      in mkTypesExcludingPrimitives kTypename_ k

    (vTypename, vDecls) = let
      vTypename_ = typename <> "Val"
      in mkTypesExcludingPrimitives vTypename_ v

    thisType = TsType typename (TsAlias $ TsMap kTypename vTypename)
    in thisType : (kDecls <> vDecls)

  U.TBigMap k v -> let
    (kTypename, kDecls) = let
      kTypename_ = typename <> "Key"
      in mkTypesExcludingPrimitives kTypename_ k

    (vTypename, vDecls) = let
      vTypename_ = typename <> "Val"
      in mkTypesExcludingPrimitives vTypename_ v

    thisType = TsType typename (TsAlias $ TsBigMap kTypename vTypename)
    in thisType : (kDecls <> vDecls)

  U.TString -> [TsType typename (TsAlias TsString)]
  U.TInt -> [TsType typename (TsAlias TsNumber)]
  U.TNat -> [TsType typename (TsAlias TsNumber)]
  U.TUnit -> [TsType typename (TsAlias TsUnit)]
  U.TSignature -> [TsType typename (TsAlias TsString)]
  U.TOperation -> [TsType typename (TsAlias TsString)]
  U.TContract _ -> [TsType typename (TsAlias TsString)]
  U.TLambda _ _ -> [TsType typename (TsAlias (TsCustom "Lambda"))]
  U.TBytes -> [TsType typename (TsAlias TsString)]
  U.TAddress -> [TsType typename (TsAlias TsString)]
  U.TKey -> [TsType typename (TsAlias TsString)]
  U.TChainId -> [TsType typename (TsAlias TsString)]
  U.TMutez -> [TsType typename (TsAlias TsNumber)]
  U.TBool -> [TsType typename (TsAlias TsBool)]
  U.TKeyHash -> [TsType typename (TsAlias TsString)]
  U.TTimestamp -> [TsType typename (TsAlias TsString)]
  U.TBls12381Fr -> [TsType typename (TsAlias TsString)]
  U.TBls12381G1 -> [TsType typename (TsAlias TsString)]
  U.TBls12381G2 -> [TsType typename (TsAlias TsString)]
  U.TNever -> [TsType typename (TsAlias TsVoid)]
  U.TTicket a -> let
    ticketFields = [("ticketer", toType U.TAddress), ("value", a), ("amount", toType U.TNat)]
    tDecls = map (mkTypesForField False) ticketFields
    in (TsType typename $ TsInterface (map fst tDecls)) : (concatMap snd tDecls)

  where

    -- Mostly same as mkTypesFor, but check if the given type
    -- is a primitive. Is yes, then skip generation of a type alias
    -- to represent it and use the primitive as the type itself.
    mkTypesExcludingPrimitives :: Typename -> U.Ty -> (TsType, [TsDecl])
    mkTypesExcludingPrimitives tn epType_ = case U.unwrapT epType_ of
      U.TString -> (TsString, [])
      U.TInt -> (TsNumber, [])
      U.TNat -> (TsNumber, [])
      U.TUnit -> (TsUnit, [])
      U.TSignature -> (TsString, [])
      U.TOperation -> (TsString, [])
      U.TContract _ -> (TsString, [])
      U.TLambda _ _ -> ((TsCustom "Lambda"), [])
      U.TBytes -> (TsString, [])
      U.TAddress -> (TsString, [])
      U.TKey -> (TsString, [])
      U.TChainId -> (TsString, [])
      U.TMutez -> (TsNumber, [])
      U.TBool -> (TsBool, [])
      U.TKeyHash -> (TsString, [])
      U.TTimestamp -> (TsString, [])
      U.TBls12381Fr -> (TsString, [])
      U.TBls12381G1 -> (TsString, [])
      U.TBls12381G2 -> (TsString, [])
      U.TNever -> (TsVoid, [])
      _ -> (TsCustom tn, mkTypesFor tn epType_)

    hasAtLeastOneNamedField :: [(Fieldname, U.Ty)] -> Bool
    hasAtLeastOneNamedField fs = isJust $ find (not . null . fst) fs

    -- Used to make an interface where the fields are numerically indexed.
    mkTypes_
      :: (U.Ty, Int)
      -> (TsType, [TsDecl])
    mkTypes_ (bt, idx) = let
      subTypename = typename <> (show idx)
      (inner, tsDcls) = mkTypesExcludingPrimitives subTypename bt
      in (inner, tsDcls)

    -- Checks the TsDecl is for a type with the given type name.
    -- If it is and the type is an Interface, then wrap the fields
    -- into top level discriminator key. This is so that we can use this type
    -- in a Union type, where the other types have same fields, which makes it
    -- hard to infer the actual type of the value from the fields alone.
    addDisciminatorFor :: Typename -> Text -> TsDecl -> [TsDecl]
    addDisciminatorFor typename_ dname a@(TsType tname (TsInterface flds)) = case typename_ == tname of
      True -> let
        innerName = typename_ <> "Item"
        inner = TsType innerName (TsInterface flds)
        in [inner, TsType tname (TsInterface [InterfaceField dname (TsCustom innerName) False])]
      False -> [a]
    addDisciminatorFor _ _ a = [a]

    -- Pack the fieldname, type, and indicate if the field is an optional type
    -- to use in the generation of an Interface.
    mkTypesForField
      :: Bool
      -> (Fieldname, U.Ty)
      -> (InterfaceField, [TsDecl])
    mkTypesForField addDisciminator (fn, bt) = let
      subTypename = typename <> (ucFirst fn)
      in case U.unwrapT bt of
        U.TOption x -> let
          (inner, tsDcls) = mkTypesExcludingPrimitives subTypename x
          withDiscriminator = case addDisciminator of
            True -> concatMap (addDisciminatorFor subTypename fn) tsDcls
            _ -> tsDcls
          in (InterfaceField fn inner True, withDiscriminator)
        _ -> let
          (inner, tsDcls) = mkTypesExcludingPrimitives subTypename bt
          withDiscriminator = case addDisciminator of
            True -> concatMap (addDisciminatorFor subTypename fn) tsDcls
            _ -> tsDcls
          in (InterfaceField
                { ifFieldName = fn
                , ifFieldType = inner
                , ifIsOptional = False
                }, withDiscriminator)

-- While creating interfaces, if there are fields where no field
-- name was found, put numeric strings for the fieldname.
indexEmptyFields :: [(Fieldname, U.Ty)] -> [(Fieldname, U.Ty)]
indexEmptyFields f = zipWith zipFn f ([0..] :: [Int])
  where
    zipFn (fn, t) idx = (bool fn (show idx) (fn == ""), t)

-- Flatten an OR type.
flattenOrs :: AnnotatedField -> [(Fieldname, U.Ty)]
flattenOrs (AnnotatedField fn bt) = case fn == noAnn of
  -- If this field have an annotation, then
  -- immediately return it. else descent into its branches.
  False -> [(unAnnotation fn, bt)]
  True -> case U.unwrapT bt of
    U.TOr lfn rfn lft rft -> let
      leftFields = case lfn == noAnn of
        True -> flattenOrs $ AnnotatedField lfn lft
        False -> [(unAnnotation lfn, lft)]
      rightFields = case rfn == noAnn of
        True -> flattenOrs $ AnnotatedField rfn rft
        False -> [(unAnnotation rfn, rft)]
      in leftFields <> rightFields
    _ -> [(unAnnotation fn, bt)]

-- Flatten a Pair type.
flattenPairs :: AnnotatedField -> [(Fieldname, U.Ty)]
flattenPairs (AnnotatedField fn bt) = case fn == noAnn of
  False -> [(unAnnotation fn, bt)]
  True -> case U.unwrapT bt of
    U.TPair lfn rfn _ _ lft rft -> let
      leftFields = case lfn == noAnn of
        True -> flattenPairs $ AnnotatedField lfn lft
        False -> [(unAnnotation lfn, lft)]
      rightFields = case rfn == noAnn of
        True -> flattenPairs $ AnnotatedField rfn rft
        False -> [(unAnnotation rfn, rft)]
      in leftFields <> rightFields
    _ -> [(unAnnotation fn, bt)]
