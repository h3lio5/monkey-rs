Should not be allowed:
- numbers cannot contain letters, identifiers cannot start with number: `1ab`, `33azg`, etc. 

Improvements:
- add jit: per-branch of ast or bytecode-to-native-code?
- add a type system
- tail recursive optimization?
- state mutability if marked as mutable

VM:
- benchmark and measure the perf diff for 1) stack size mutation i.e., stack.push()/.pop() 2) stack index based access by init it during creation with Object::Null.