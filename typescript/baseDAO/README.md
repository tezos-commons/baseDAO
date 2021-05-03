# BaseDAO Typescript API

This package implements a Typescript interface to interact with the BaseDAO
contract.

## Build instructions

You will need the `yarn` package manager to build this library. It can be built
as follows.

```sh
yarn install
yarn tsc
```

### Generating Typescript interface types

The Typescript types representing the BaseDAO contract parameter can be generated
automatically by using the `make typescript` command.
The files will be saved to the [/src/generated](src/generated) directory.

## Usage

To add this library as a dependency to your typescript app, you can use `yarn add` by specifiying
path to it as follows.


```sh
yarn add file:/path-to-this-repo/typescript/baseDAO/
```

Then you will need the contract address of the BaseDAO contract and the secret key
of the sender to start using this library. A sample usage can be seen in the snippets below.

```
import { BaseDAOContract } from "basedao-sdk";

let sender_secret_key : string = 'edsk36njbEALasoBeisebfAZgXXMECPL3xt9KLuiQqGB3tEFZrMq7e';
let contract_address : string =  'KT1VjT5AiAkek74kUyS5t41dmTM3abGTv9Kc';

const BaseDAO = new BaseDAOContract
  ( 'https://mainnet-tezos.giganode.io'
  , sender_secret_key
  , contract_address
  );

// call some entrypoints

BaseDAO
  .balance_of({requests: [{owner: "tz1ZvZCqjaBLEyJLYHCfzq6B8MCFu2SRjzJG", token_id: 0}], callback: "KT1TQSvKjZuQGP4jQiuGU6XfkPh9rwb6SGDP"})
  .catch(err => console.log(JSON.stringify(err), null, 2));

BaseDAO
  .propose({frozen_token: 10, proposal_metadata: new Map([["key", "FF"]])})
  .catch(err => console.log(JSON.stringify(err), null, 2));

BaseDAO
  .get_vote_permit_counter({voidParam: {}, voidResProxy: [{prim: 'DUP'}, {prim: 'DROP'}]})
  .catch(err => console.log(JSON.stringify(err), null, 2));
```

### Contract validation

The target BaseDAO contract's storage and parameter can be validated against a known schema to make sure that
we are talking to a compatible contract.

You can obtain a parameter and storage schema with:

```
let contractStorageSchema = await BaseDAO.storageSchema();
let contractParameterSchema = await BaseDAO.parameterSchema();
```
and you can verify a contract schema with:

```
BaseDAO.ensureStorageSchema(contractStorageSchema).catch(err => console.log(err));
BaseDAO.ensureParamSchema(contractParameterSchema).catch(err => console.log(err));
```

If the schemas does not match, the actual schema that was found will be included as part of the
error.
