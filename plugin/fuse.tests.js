const fb = require('fuse-box');

const FuseBox = fb.FuseBox;

const fuse = FuseBox.init({
    homeDir: "src",
    outFile: "./tests/tests.js",
    tsConfig: "tsconfig.json",
    sourceMaps: true
});

fuse.bundle({
  "tests/lib.spec.js": ">lib.spec.ts"
});


