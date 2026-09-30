@:generic
class Box<T> {
	public var value:T;

	public function new(v:T) {
		this.value = v;
	}

	public function get():T {
		return this.value;
	}
}

class GenHx {
	static function main():Void {
		var b = new Box<Int>(41);
		var c = new Box<String>("hi");
		if (b.get() == 41) {
			trace("gen ok");
		} else {
			trace("gen bad");
		}
		trace("gen2 ok");
	}
}
