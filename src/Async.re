type t('a) = ('a => unit) => unit;

let consume = (task: t('a), fin) => task(fin);

let value = (value, fin) => fin(value);

let map = (task: t('a), mapFn: 'a => 'b, fin) =>
  task(value => fin(mapFn(value)));

let flatMap = (task: t('a), flatMapFn: 'a => t('b), fin) =>
  task(value => flatMapFn(value, fin));

let tap = (task: t('a), tapFn: 'a => unit, fin) =>
  task(value => {
    tapFn(value);
    fin(value);
  });

type bothState('a, 'b) =
  | Empty
  | Left('a)
  | Right('b)
  | Finished;

let both = (leftTask: t('a), rightTask: t('b), fin) => {
  let state = ref(Empty);

  leftTask(leftValue =>
    switch (state^) {
    | Finished
    | Left(_) => ()
    | Empty => state := Left(leftValue)
    | Right(rightValue) =>
      state := Finished;
      fin((leftValue, rightValue));
    }
  );

  rightTask(rightValue =>
    switch (state^) {
    | Finished
    | Right(_) => ()
    | Empty => state := Right(rightValue)
    | Left(leftValue) =>
      state := Finished;
      fin((leftValue, rightValue));
    }
  );
};
