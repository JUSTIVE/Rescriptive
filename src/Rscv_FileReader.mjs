// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";

function readAllFile(filePath) {
  return Fs.readFileSync(filePath, "utf8");
}

function readFileLine(filePath) {
  return Fs.readFileSync(filePath, "utf8").split("\n");
}

export {
  readAllFile ,
  readFileLine ,
  
}
/* fs Not a pure module */
