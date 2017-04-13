const fb = require('fuse-box');

const FuseBox = fb.FuseBox;

const fuse = FuseBox.init({
    homeDir: "src",
    outFile: "./tests/tests.js",
    tsConfig: "tsconfig.json",
    sourceMaps: true
});

fuse.bundle({
  "tests/store.spec.js": ">store/store.spec.ts",
  "tests/liftReducer.spec.js": ">reducers/liftReducer.spec.ts",
  "tests/addPageEvent.spec.js": ">reducers/pages/actions/addPageEvent.action.spec.ts",
  "tests/pageShownEvent.spec.js": ">reducers/pages/actions/pageShownEvent.action.spec.ts",
  "tests/action.creators.spec.js": ">reducers/pages/actions/action.creators.spec.ts",
  "tests/pages.reducer.spec.js": ">reducers/pages/pages.reducer.spec.ts",
  "tests/popup.view.spec.js": ">views/popup.view.spec.ts",
  "tests/badge.view.spec.js": ">views/badge.view.spec.ts",
});


