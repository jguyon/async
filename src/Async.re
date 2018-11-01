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
