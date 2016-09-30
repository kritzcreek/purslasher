var keys = {}
exports.attachKeyListeners = function () {
    window.addEventListener('keydown', function(ev) {
        if ([37, 38, 39].indexOf(ev.keyCode) !== -1){
            ev.preventDefault();
        }
        keys[ev.keyCode] = true
    })
    window.addEventListener('keyup', function(ev) {
        keys[ev.keyCode] = false})}

exports.isKeyPressed = function (keyCode) {
    return function() {
        return !!keys[keyCode]}}

exports.requestAnimationFrame = function (cb) {
    return function () {
        window.requestAnimationFrame(
            function(timestamp){
                cb(timestamp)()})}}
