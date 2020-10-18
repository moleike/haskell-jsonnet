local test = [
  {
    foo: "test1",
    bar: {
      baz: {
        qux: {
          quux: $.foo
        }
      }
    }
  },
  {
    foo: "test2",
    bar: {
      baz: {
        qux: {
          quux: $["foo"]
        }
      }
    }
  }
];

test
