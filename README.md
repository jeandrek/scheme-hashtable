# scheme-hashtable
An implementation of a hash table written in portable Scheme

## API Reference
### Procedure: `make-hash-table`
Create and return a new hash table using `equal?` for comparison of keys.
All hash tables are mutable.
### Procedure: `make-hash-tableq`
Create and return a new hash table using `eq?` for comparison of keys.
### Procedure: `make-hash-tablev`
Create and return a new hash table using `eqv?` for comparison of keys.
