import sa.net.Tcp;

class NetHx {
	static function main():Void {
		var l:UInt = Tcp.listen(0);
		var p:Int = Tcp.boundPort(l);
		Tcp.close(l);
		if (p > 0) {
			trace("net ok");
		} else {
			trace("net bad");
		}
	}
}
