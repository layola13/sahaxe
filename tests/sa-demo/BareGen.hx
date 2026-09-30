@:generic
class GBox<T> {
	public var value:T;

	public function new(v:T) {
		this.value = v;
	}

	public function get():T {
		return this.value;
	}
}

// Bare (non-@:generic) Box: the SA target emits an actionable
// SA-NOTE pointing at @:generic instead of lowering (see SA_TARGET).
class Wrap<T> {
	public var v:T;

	public function new(v:T) {
		this.v = v;
	}
}

class BareGen {
	static function main():Void {
		var b = new GBox<Int>(41);
		if (b.get() == 41) {
			trace("generic ok");
		} else {
			trace("generic bad");
		}
		var w = new Wrap<Int>(7);
		trace("bare noted");
	}
}
