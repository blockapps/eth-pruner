var Mocha = require('mocha');
var fs = require('fs');
var mocha = new Mocha({});

mocha.addFile('./setPrivateStorage.test.js');

mocha.run()
    .on('test', function (test) {
        console.log('Test started: ' + test.title);
    })
    .on('test end', function (test) {
        console.log('Test done: ' + test.title);
    })
    .on('pass', function (test) {
        console.log('Test passed');
        process.exit(0);
    })
    .on('fail', function (test, err) {
        console.log('Test fail');
        console.log(err);
        process.exit(1);
    })
    .on('end', function () {
        console.log('All done');
        process.exit(0);
    });