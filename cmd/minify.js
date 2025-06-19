#!/usr/bin/env node
const fs = require("fs");
const { transformCode, transformElmCode } = require("./transformer");
const codeAll = 
    [ fs.readFileSync(`./build-generated/GENERATED-meta.js`, "utf8")
    , transformElmCode(fs.readFileSync(`./build-generated/${process.argv[5]}`, "utf8"))
    , fs.readFileSync(`./public/${process.argv[6]}`, "utf8")
    ].join(";");
const result = transformCode(
    { code: codeAll
    , name: process.argv[2]
    , version: process.argv[3]
    , additionalInfo: process.argv[4]
    , withCopyright: true
    }
);
fs.writeFileSync(`./build-generated/${process.argv[7]}`, result.code);