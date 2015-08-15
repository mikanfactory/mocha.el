var assert = require("assert");

describe('Array', function() {
  describe('#indexOf()', function () {
    it('should return -1 when the value is not present', function () {
      assert.equal(-1, [1,2,3].indexOf(5));
      assert.equal(-1, [1,2,3].indexOf(0));
    });
  });

  describe('#join()', function () {
    it('should join list', function () {
      assert.equal("foo bar bazz", ["foo", "bar", "bazz"].join(' '));
      assert.equal("Land of Lisp is awesome", ["Land of Lisp", "is", "awesome"].join(' '));
    });
  });
});
