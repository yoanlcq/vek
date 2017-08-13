# This script takes care of testing your crate

set -ex

main() {
    cross build --target $TARGET
    cross build --target $TARGET --release

    if [ ! -z $DISABLE_TESTS ]; then
        return
    fi

    if [ ! $AVOID_DOC_TESTS ]; then
        cross test --doc --no-default-features --no-fail-fast --target $TARGET
        cross test --doc --no-default-features --no-fail-fast --target $TARGET --release
    fi

    cross test --lib --no-fail-fast --target $TARGET
    cross test --lib --no-fail-fast --target $TARGET --release

    #cross run --target $TARGET
    #cross run --target $TARGET --release
}

# we don't run the "test phase" when doing deploys
if [ -z $TRAVIS_TAG ]; then
    main
fi
