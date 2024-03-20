# .rvn (Raven)

RVN is a serialization format based of [Roc].
Any Roc value is valid RVN.

This means you can serialize to and from files with contents like this:

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
