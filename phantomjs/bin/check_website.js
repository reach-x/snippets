const system = require('system');
const timestamp = Date.now();
const address = system.args[1];
const webPage = require('webpage');
const page = webPage.create();

page.onError = function (msg, trace) {
    console.log("onError called")

    var msgStack = ['ERROR: ' + msg];

    if (trace && trace.length) {
        msgStack.push('TRACE:');
        trace.forEach(function (t) {
            msgStack.push(' -> ' + t.file + ': ' + t.line + (t.function ? ' (in function "' + t.function + '")' : ''));
        });
    }

    console.error(msgStack.join('\n'));

};


page.open(address, function (status) {
    if (status !== 'success') {
        console.log('FAIL to load the address');
    } else {
        var title = page.evaluate(function () {
            return document.title;
        });

        timestamp = Date.now() - timestamp;
        filename = getHostname(address) + '.png';

        console.log('Page title is ' + title);
        console.log('Loading ' + system.args[1]);
        console.log('Loading time ' + timestamp + ' msec');
        console.log('Filename: ' + filename);

        page.render(filename);
    }
    phantom.exit();
});