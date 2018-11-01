type t('a) = ('a => unit) => unit;

let consume: (t('a), 'a => unit) => unit;

let value: 'a => t('a);

let map: (t('a), 'a => 'b) => t('b);

let flatMap: (t('a), 'a => t('b)) => t('b);

let tap: (t('a), 'a => unit) => t('a);
