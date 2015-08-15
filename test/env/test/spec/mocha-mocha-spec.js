var assert = require("assert");
describe('Array', function() {
  describe('#join()', function () {
    it('should join list', function () {
      assert.equal("foo bar bazz", ["foo", "bar", "bazz"].join(' '));
      assert.equal("Land of Lisp is awesome", ["Land of Lisp", "is", "awesome"].join(' '));
    });
  });
});
