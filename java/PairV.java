
public class PairV implements Value {
	private Value left;
	private Value right;

	public PairV(Value left, Value right) {
		this.left = left;
		this.right = right;
	}

	public Value getLeft() {
		return left;
	}

	public Value getRight() {
		return right;
	}

	@Override
	public String toString() {
		return "(" + left.toString() + ", " + right.toString() + ")";
	}
}
