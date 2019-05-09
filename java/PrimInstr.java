
public class PrimInstr implements Instr {

	private PrimOp primOp;

	public PrimInstr(PrimOp primOp) {
		this.primOp = primOp;
	}

	private void executeFst(Config config, PairV pair) {
		config.setValue(pair.getLeft());
		config.nextInstruction();
	}

	private void executeSnd(Config config, PairV pair) {
		config.setValue(pair.getRight());
		config.nextInstruction();
	}

	private void executeAdd(Config config, PairV pair) {
		Value left = pair.getLeft();
		Value right = pair.getRight();

		if (!(left instanceof IntV) || !(right instanceof IntV))
			throw new IllegalStateException("Opération sur des non-entiers !");

		IntV op1 = (IntV) left;
		IntV op2 = (IntV) right;

		config.setValue(new IntV(op1.getValue() + op2.getValue()));
		config.nextInstruction();
	}

	@Override
	public void execute(Config config) {
		Value value = config.getValue();

		if (!(value instanceof PairV))
			throw new IllegalStateException("Un opérateur est appliqué à une valeurr qui n'est pas une paire !");

		PairV pair = (PairV) value;

		switch (primOp) {
		case FST:
			executeFst(config, pair);
			break;
		case SND:
			executeSnd(config, pair);
			break;
		case ADD:
			executeAdd(config, pair);
			break;
		case SUB:
			executeSub(config, pair);
			break;
		case MUL:
			executeMul(config, pair);
			break;
		case DIV:
			executeDiv(config, pair);
			break;
		case MOD:
			executeMod(config, pair);
			break;
		case EQ:
			executeEQ(config, pair);
			break;
		case GE:
			executeGE(config, pair);
			break;
		case GT:
			executeGT(config, pair);
			break;
		case LE:
			executeLE(config, pair);
			break;
		case LT:
			executeLT(config, pair);
			break;
		case NE:
			executeNE(config, pair);
			break;
		}
	}
}
