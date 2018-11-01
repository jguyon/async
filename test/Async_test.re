open Jest;

let delay = (~ms=0, value, fin) => {
  let _ = Js.Global.setTimeout(() => fin(value), ms);
  ();
};

describe("Async", () => {
  open Expect;

  describe(".consume", () =>
    testAsync("runs the task", fin =>
      delay("value")
      ->Async.consume(value => expect(value) |> toEqual("value") |> fin)
    )
  );

  describe(".value", () =>
    testAsync("produces the given value", fin =>
      Async.value("value")
      ->Async.consume(value => expect(value) |> toEqual("value") |> fin)
    )
  );

  describe(".map", () =>
    testAsync("produces the created value", fin =>
      delay("42")
      ->Async.map(value => int_of_string(value))
      ->Async.consume(value => expect(value) |> toEqual(42) |> fin)
    )
  );

  describe(".flatMap", () =>
    testAsync("produces the value from the created task", fin =>
      delay("42")
      ->Async.flatMap(value => delay(int_of_string(value)))
      ->Async.consume(value => expect(value) |> toEqual(42) |> fin)
    )
  );

  describe(".tap", () => {
    testAsync("gives the produced value to the given function", fin => {
      let state = ref(None);

      delay("value")
      ->Async.tap(v => state := Some(v))
      ->Async.consume(_ => expect(state^) |> toEqual(Some("value")) |> fin);
    });

    testAsync("leaves the produced value unchanged", fin =>
      delay("value")
      ->Async.tap(_ => ())
      ->Async.consume(value => expect(value) |> toEqual("value") |> fin)
    );
  });

  describe(".both", () => {
    testAsync("produces values when left task finishes first", fin =>
      Async.both(Async.value(42), delay("value"))
      ->Async.consume(value =>
          expect(value) |> toEqual((42, "value")) |> fin
        )
    );

    testAsync("produces values when right task finishes first", fin =>
      Async.both(delay(42), Async.value("value"))
      ->Async.consume(value =>
          expect(value) |> toEqual((42, "value")) |> fin
        )
    );

    testAsync("runs tasks in order", fin => {
      let values = ref([]);

      let addValue = (value, fin) => {
        values := values^ @ [value];
        fin();
      };

      Async.both(addValue("left"), addValue("right"))
      ->Async.consume(_ =>
          expect(values^) |> toEqual(["left", "right"]) |> fin
        );
    });
  });

  describe(".all", () => {
    testAsync("produces the empty list given no tasks", fin =>
      Async.all([])
      ->Async.consume(value => expect(value) |> toEqual([]) |> fin)
    );

    testAsync("produces values from all tasks in order", fin =>
      Async.all([delay(1), Async.value(2), delay(3)])
      ->Async.consume(value => expect(value) |> toEqual([1, 2, 3]) |> fin)
    );

    testAsync("runs tasks in order", fin => {
      let values = ref([]);

      let addValue = (value, fin) => {
        values := values^ @ [value];
        fin();
      };

      Async.all([addValue(1), addValue(2), addValue(3)])
      ->Async.consume(_ => expect(values^) |> toEqual([1, 2, 3]) |> fin);
    });
  });

  describe(".pick", () => {
    testAsync("produces the value from the first task to finish", fin =>
      Async.pick([delay(1), Async.value(2), delay(3)])
      ->Async.consume(value => expect(value) |> toEqual(2) |> fin)
    );

    testAsync("runs tasks in order", fin => {
      let values = ref([]);

      let addValue = (value, fin) => {
        values := values^ @ [value];
        fin();
      };

      Async.pick([addValue(1), addValue(2), addValue(3)])
      ->Async.flatMap(() => delay())
      ->Async.consume(() => expect(values^) |> toEqual([1, 2, 3]) |> fin);
    });
  });
});
