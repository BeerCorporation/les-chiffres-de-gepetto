To compile, add a Config.fs file at the top-root of the project with the following

```
namespace Lcdg

module Config =
    [<Literal>]
    let dbString = // Connection string to DB
```