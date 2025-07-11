class DataInner {
	int inner;
	DataInner(int inner) {
		this.inner = inner;
	}
}
class DataOuter {
	DataInner outer;
	DataOuter(DataInner outer) {
		this.outer = outer;
	}
}
public class MapNestedAssign {
	static DataOuter data = new DataOuter(new DataInner(1));
	public static void main(String[] args) {
	data.outer.inner = 2;
	System.out.println(data.outer.inner);
	}
}
