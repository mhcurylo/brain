const fb = require('fuse-box');

const FuseBox = fb.FuseBox;

const fuse = FuseBox.init({
    homeDir: "src",
    outFile: 'bundle.js',
    tsConfig: "tsconfig.json",
    sourceMaps: true
});

fuse.bundle({
  "chrome/background.js": ">background.ts",
  "chrome/popup.js": ">popup.ts"
});


