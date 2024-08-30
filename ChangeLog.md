# Changelog for propeller

## 0.1.0.0 (2020-02-10)

- Initial release

## 0.2.0.0 (2024-06-10)

- Added an immutable propagator implementation.

## 0.3.0.0 (2024-07-31)

- Removed cycle detection because of performance and flexibility.
- Exposed some internals for better usability in certain situations.

## 0.4.0.0 (2024-08-31)

- Added a propagator implementation with cells of heterogeneous types.
- Allowed an initial cell value to depend on its cell key.