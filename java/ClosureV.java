import java.util.LinkedList;
import java.util.Collection;

public class ClosureV implements Value {

	private Value environment;
	private LinkedList<Instr> code;

	public ClosureV(Collection<Instr> code, Value environment) {
		/* Values are immutable, we can use them directly */
		this.environment = environment;
		this.code = new LinkedList<>(code);
	}

	public Value getEnvironment() {
		return environment;
	}

	/*
	 * We return the LinkedList as an Iterable to guard it against
	 * modifications.
	 */
	public Iterable<Instr> getCode() {
		return code;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();

		builder.append("([");

		if (code.size() > 0)
			builder.append(code.get(0).toString());

		for (int i = 1; i < code.size(); i++) {
			builder.append(code.get(i).toString());
			builder.append("; ");
		}

		builder.append("], ");
		builder.append(environment.toString());
		builder.append(")");

		return builder.toString();
	}
}
