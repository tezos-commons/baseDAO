// Instantiation-specific stored data
type contract_extra = (string, bytes) big_map

let default_extra : contract_extra = (Big_map.empty : contract_extra)
