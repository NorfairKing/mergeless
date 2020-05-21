# Changelog for mergeless

## Unreleased changes

## [0.3.0.0] - 2020-05-21

### Added

* mergeSyncResponseCustom
* emptySyncResponse: in case you want to do the sync response creation yourself

### Changed

* Another parameter: the ClientId, so that you can use an sqlid as your client id
  This likely means that none of your code from mergeless-0.2 still compiles but it's easy to fix.

## [0.2.0.1] - 2020-02-13

* Fixed the benchmarks

## [0.2.0.0] - 2020-02-12

* Got rid of the UTCTimes. The client should take care of that.
