import java.util.*;

public class Config {
	private Value value;
	private List<Instr> code;
	private List<StackElem> stack;

	public Config (Value value, List<Instr> code, List<StackElem> stack) {
		this.value = value;
		this.code = code;
		this.stack = stack;
	}

	public Value getValue() {
		return value;
	}

	public void setValue(Value value) {
		this.value = value;
	}

	List<Instr> getCode() {
		return code;
	}

	public void setCode(List<Instr> code) {
		this.code = code;
	}

	List<StackElem> getStack() {
		return stack;
	}

	public void setStack(List<StackElem> stack) {
		this.stack = stack;
	}

	public void nextInstruction() {
		code.remove(0);
	}

	// one-step execution
	public boolean exec_step() {
		// to be implemented
		return true;
	}

	// run to completion
	public void exec() {
		// to be implemented
	}

	// run for n steps
	public void step(int n) {
		// to be implemented
	}
}
