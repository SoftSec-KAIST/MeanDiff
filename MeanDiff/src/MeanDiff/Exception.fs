namespace MeanDiff

exception NotImplemented

exception FileNotFound  // MeanDiff.fs

exception InsufficientOption  // CmdOpt.fs
exception InvalidOption       // CmdOpt.fs
exception DuplicatedOption    // CmdOpt.fs

exception EmptyList           // Graph.fs
exception InvalidIRStructure  // Graph.fs

exception FieldNotFound       // JSONParse.fs
exception InvalidTypeField    // JSONParse.fs
exception InvalidSubTypeField // JSONParse.fs
exception InvalidArgsLength   // JSONParse.fs

exception InvalidMessage  // MultProc.fs

exception IncomparableMemoryWarning // SMTSolver.fs
exception MemoryNumberMismatch      // SMTSolver.fs
exception VariableNumberMismatch    // SMTSolver.fs
exception SolverUnknown             // SMTSolver.fs

exception AddressNotSet       // Symbolic.fs
exception ConditionNotSet     // Symbolic.fs
exception InvalidDestination  // Symbolic.fs
exception LoopDetected        // Symbolic.fs
exception TimeOut             // Symbolic.fs

exception InvalidCastSize       // Type.fs
exception OperandSizeMismatch   // Type.fs
exception VariableSizeMismatch  // Type.fs

exception OperandLengthUnmatched  // Utils.fs
