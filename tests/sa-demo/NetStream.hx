import sa.net.Tcp;
import sa.net.Udp;

class NetStream {
	static function main():Void {
		var l:UInt = Tcp.listen(0);
		var p:Int = Tcp.boundPort(l);
		var s:UInt = Tcp.connect("127.0.0.1", p);
		var w:Int = Tcp.write(s, "hi");
		Tcp.setReadTimeout(s, 100);
		Tcp.closeStream(s);
		Tcp.close(l);
		var u:UInt = Udp.bind(0);
		var q:Int = Udp.port(u);
		Udp.connect(u, "127.0.0.1", q);
		var n:Int = Udp.send(u, "hey");
		Udp.setReadTimeout(u, 500);
		var m:String = Udp.recv(u, 64);
		Udp.close(u);
		if (w == 2 && n == 3 && m == "hey" && p > 0 && q > 0) {
			trace("netstream ok");
		} else {
			trace("netstream bad");
		}
	}
}
