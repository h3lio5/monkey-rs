Should not be allowed:
- numbers cannot contain letters, identifiers cannot start with number: `1ab`, `33azg`, etc. 

considerations:
- token field can be removed from the AST node types?
- take care of garbage collection!

Improvements:
- does types have truthy values? can truthy values be used in if expressions?
- add jit: per-branch of ast or bytecode-to-native-code?
- add a type system
- tail recursive optimization?