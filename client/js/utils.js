

define(function () {
    function randInt(min, max) {
          return Math.floor(Math.random() * (max - min)) + min;
    }
    return {
        randInt: randInt
    };
});
