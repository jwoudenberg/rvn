# .rvn (Raven)

Raven is to [Roc] as JSON is to JavaScript.
Any Roc value that does not contain functions is valid Raven.

This means you can serialize to and from data like this:

```roc
{
    language: "Roc",
    tags: [Fast, Friendly, Functional],
    color: 0x7c38f5, # supports hex digits, comments too!
}
```

Many thanks for Luke Boswell for [roc-json][], which served as a great example showing how to write a library like this!

[Roc]: https://roc-lang.org
[roc-json]: https://github.com/lukewilliamboswell/roc-json
