open Belt;
open Jest;

let delay = (~ms=0, value, fin) => {
  let _ = Js.Global.setTimeout(() => fin(value), ms);
  ();
};

let delayOk = (~ms=?, value) => delay(~ms?, Result.Ok(value));

let delayError = (~ms=?, error) => delay(~ms?, Result.Error(error));

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

  describe(".ok", () =>
    testAsync("succeeds with the given value", fin =>
      Async.ok("value")
      ->Async.consume(value =>
          expect(value) |> toEqual(Result.Ok("value")) |> fin
        )
    )
  );

  describe(".error", () =>
    testAsync("fails with the given error", fin =>
      Async.error("error")
      ->Async.consume(value =>
          expect(value) |> toEqual(Result.Error("error")) |> fin
        )
    )
  );

  describe(".map", () =>
    testAsync("produces the created value", fin =>
      delay("42")
      ->Async.map(value => int_of_string(value))
      ->Async.consume(value => expect(value) |> toEqual(42) |> fin)
    )
  );

  describe(".mapOk", () => {
    testAsync("succeeds with the created value on success", fin =>
      delayOk("42")
      ->Async.mapOk(value => int_of_string(value))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok(42)) |> fin
        )
    );

    testAsync("leaves the produced result unchanged on failure", fin =>
      delayError("42")
      ->Async.mapOk(value => int_of_string(value))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("42")) |> fin
        )
    );
  });

  describe(".mapError", () => {
    testAsync("fails with the created error on failure", fin =>
      delayError("42")
      ->Async.mapError(error => int_of_string(error))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error(42)) |> fin
        )
    );

    testAsync("leaves the produced result unchanged on success", fin =>
      delayOk("42")
      ->Async.mapError(error => int_of_string(error))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok("42")) |> fin
        )
    );
  });

  describe(".flatMap", () =>
    testAsync("produces the value from the created task", fin =>
      delay("42")
      ->Async.flatMap(value => delay(int_of_string(value)))
      ->Async.consume(value => expect(value) |> toEqual(42) |> fin)
    )
  );

  describe(".flatMapOk", () => {
    testAsync("produces the result from the created task on success", fin =>
      delayOk("42")
      ->Async.flatMapOk(value => delay(Result.Ok(int_of_string(value))))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok(42)) |> fin
        )
    );

    testAsync("leaves the produced result unchanged on failure", fin =>
      delayError("42")
      ->Async.flatMapOk(value => delay(Result.Ok(int_of_string(value))))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("42")) |> fin
        )
    );
  });

  describe(".flatMapError", () => {
    testAsync("produces the result from the created task on failure", fin =>
      delayError("42")
      ->Async.flatMapError(error =>
          delay(Result.Error(int_of_string(error)))
        )
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error(42)) |> fin
        )
    );

    testAsync("leaves the produced result unchanged on success", fin =>
      delayOk("42")
      ->Async.flatMapError(error =>
          delay(Result.Error(int_of_string(error)))
        )
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok("42")) |> fin
        )
    );
  });

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

  describe(".tapOk", () => {
    testAsync("gives the produced value to the given function on success", fin => {
      let state = ref(None);

      delayOk("value")
      ->Async.tapOk(v => state := Some(v))
      ->Async.consume(_ => expect(state^) |> toEqual(Some("value")) |> fin);
    });

    testAsync("does not run the given function on failure", fin => {
      let state = ref(None);

      delayError("error")
      ->Async.tapOk(v => state := Some(v))
      ->Async.consume(_ => expect(state^) |> toEqual(None) |> fin);
    });

    testAsync("leaves the produced result unchanged on success", fin =>
      delayOk("value")
      ->Async.tapOk(_ => ())
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok("value")) |> fin
        )
    );

    testAsync("leaves the produced result unchanged on failure", fin =>
      delayError("error")
      ->Async.tapOk(_ => ())
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("error")) |> fin
        )
    );
  });

  describe(".tapError", () => {
    testAsync("gives the produced error to the given function on failure", fin => {
      let state = ref(None);

      delayError("error")
      ->Async.tapError(e => state := Some(e))
      ->Async.consume(_ => expect(state^) |> toEqual(Some("error")) |> fin);
    });

    testAsync("does not run the given function on success", fin => {
      let state = ref(None);

      delayOk("value")
      ->Async.tapError(e => state := Some(e))
      ->Async.consume(_ => expect(state^) |> toEqual(None) |> fin);
    });

    testAsync("leaves the produced result unchanged on failure", fin =>
      delayError("error")
      ->Async.tapError(_ => ())
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("error")) |> fin
        )
    );

    testAsync("leaves the produced result unchanged on success", fin =>
      delayOk("value")
      ->Async.tapError(_ => ())
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok("value")) |> fin
        )
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

  describe(".bothOk", () => {
    testAsync("succeeds with values when left then right succeed", fin =>
      Async.bothOk(Async.ok(42), delayOk("value"))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok((42, "value"))) |> fin
        )
    );

    testAsync("succeeds with values when right then left succeed", fin =>
      Async.bothOk(delayOk(42), Async.ok("value"))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok((42, "value"))) |> fin
        )
    );

    testAsync("fails with left error when left then right fail", fin =>
      Async.bothOk(Async.error("left"), delayError("right"))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("left")) |> fin
        )
    );

    testAsync("fails with right error when right then left fail", fin =>
      Async.bothOk(delayError("left"), Async.error("right"))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("right")) |> fin
        )
    );

    testAsync("fails with left error when left fails then right succeeds", fin =>
      Async.bothOk(Async.error("error"), delayOk(42))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("error")) |> fin
        )
    );

    testAsync("fails with left error when right succeeds then left fails", fin =>
      Async.bothOk(delayError("error"), Async.ok(42))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("error")) |> fin
        )
    );

    testAsync(
      "fails with right error when left succeeds then right fails", fin =>
      Async.bothOk(Async.ok(42), delayError("error"))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("error")) |> fin
        )
    );

    testAsync(
      "fails with right error when right fails then left succeeds", fin =>
      Async.bothOk(delayOk(42), Async.error("error"))
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error("error")) |> fin
        )
    );

    testAsync("runs tasks in order", fin => {
      let values = ref([]);

      let addValue = (value, fin) => {
        values := values^ @ [value];
        fin(Result.Ok());
      };

      Async.bothOk(addValue("left"), addValue("right"))
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

  describe(".allOk", () => {
    testAsync("succeeds with the empty list given no tasks", fin =>
      Async.allOk([])
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok([])) |> fin
        )
    );

    testAsync("succeeds with values from all tasks in order", fin =>
      Async.allOk([delayOk(1), Async.ok(2), delayOk(3)])
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Ok([1, 2, 3])) |> fin
        )
    );

    testAsync("fails with error from the first failing task", fin =>
      Async.allOk([
        delayError(1),
        Async.ok(2),
        Async.error(3),
        delayOk(4),
      ])
      ->Async.consume(result =>
          expect(result) |> toEqual(Result.Error(3)) |> fin
        )
    );

    testAsync("runs tasks in order", fin => {
      let values = ref([]);

      let addValue = (value, fin) => {
        values := values^ @ [value];
        fin(Result.Ok());
      };

      Async.allOk([addValue(1), addValue(2), addValue(3)])
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
