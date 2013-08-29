using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Runtime.InteropServices;
using VOperand = MijStuff.VM2.Asm.Operand;



using System;

namespace MijStuff.VM2.Asm {



public class Parser {
    public const int _EOF = 0;
    public const int _dec_number = 1;
    public const int _hex_number = 2;
    public const int _string = 3;
    public const int _ident = 4;
    public const int _subident = 5;
    public const int maxT = 25;

    const bool T = true;
    const bool x = false;
    const int minErrDist = 2;
    
    public Scanner scanner;
    public Errors  errors;

    public Token t;    // last recognized token
    public Token la;   // lookahead token
    int errDist = minErrDist;

[StructLayout(LayoutKind.Explicit, Size = 4)]
    public struct FloatInt {
        [FieldOffset(0)]
        public int IntVal;
        [FieldOffset(0)]
        public float FVal;
    }

    int TypeDec(string tv) { switch(tv) {
        case "b": return 1;
        case "h": return 2;
        case "w": return 4;
        default: return 0;
    }}

    int InstrDec(string ix) {
        Instr val;
        ix = ix.Replace("_","ಠ_ಠ").Replace(".", "_");
        var succ = Instr.TryParse(ix, true, out val);
        return (succ ? (int)val : -1);
    }

    int DecNumber(string val) {
        if(val.EndsWith("f") || val.EndsWith("F")) {
            var Conv = new FloatInt();
            Conv.FVal = Convert.ToSingle(val.TrimEnd('f','F'));
            return Conv.IntVal;
        } return Convert.ToInt32(val);
    }

    string EscapeChars(string input) {
        return Regex.Replace(input, @"\\(.)", delegate(Match mx) {
            switch(mx.Groups[1].Value) {
                case "\\": return "\\";
                case "q": return "\"";
                default: return "";
            }
        });
    }

    public List<Instruction> instructions;
    public Dictionary<string, Label> labels;
    private int instr_idx = 0;



    public Parser(Scanner scanner) {
        this.scanner = scanner;
        errors = new Errors();
    }

    void SynErr (int n) {
        if (errDist >= minErrDist) errors.SynErr(la.line, la.col, n);
        errDist = 0;
    }

    public void SemErr (string msg) {
        if (errDist >= minErrDist) errors.SemErr(t.line, t.col, msg);
        errDist = 0;
    }
    
    void Get () {
        for (;;) {
            t = la;
            la = scanner.Scan();
            if (la.kind <= maxT) { ++errDist; break; }

            la = t;
        }
    }
    
    void Expect (int n) {
        if (la.kind==n) Get(); else { SynErr(n); }
    }
    
    bool StartOf (int s) {
        return set[s, la.kind];
    }
    
    void ExpectWeak (int n, int follow) {
        if (la.kind == n) Get();
        else {
            SynErr(n);
            while (!StartOf(follow)) Get();
        }
    }


    bool WeakSeparator(int n, int syFol, int repFol) {
        int kind = la.kind;
        if (kind == n) {Get(); return true;}
        else if (StartOf(repFol)) {return false;}
        else {
            SynErr(n);
            while (!(set[syFol, kind] || set[repFol, kind] || set[0, kind])) {
                Get();
                kind = la.kind;
            }
            return StartOf(syFol);
        }
    }

    
    void Constant(ref int? value) {
        if (la.kind == 2) {
            Get();
            value = Convert.ToInt32(t.val,16); 
        } else if (la.kind == 1) {
            Get();
            value = DecNumber(t.val); 
        } else SynErr(26);
    }

    void Stack(ref int? value) {
        Expect(6);
        value = null; 
    }

    void MOperand(Instruction inst) {
        bool p=false, ff=false; int? val=null; string tt="w"; 
        if (la.kind == 7) {
            Get();
            p=true; 
        }
        if (la.kind == 1 || la.kind == 2 || la.kind == 8) {
            if (la.kind == 8) {
                Get();
                ff=true; 
            }
            Constant(ref val);
        } else if (la.kind == 6) {
            Stack(ref val);
        } else SynErr(27);
        if (la.kind == 9) {
            Get();
            if (la.kind == 10) {
                Get();
            } else if (la.kind == 11) {
                Get();
            } else if (la.kind == 12) {
                Get();
            } else SynErr(28);
            tt=t.val; 
            Expect(13);
        }
        if(val==null) inst.Operands.Add(VOperand.FromStack(TypeDec(tt),p));
        else inst.Operands.Add(VOperand.FromNumber(val.Value,TypeDec(tt),p,ff)); 
    }

    void LOperand(Instruction inst) {
        bool p=false; string lb; string tt="w"; 
        if (la.kind == 14) {
            Get();
        } else if (la.kind == 15) {
            Get();
            p=true; 
        } else SynErr(29);
        Expect(4);
        lb=t.val; 
        if (la.kind == 9) {
            Get();
            if (la.kind == 10) {
                Get();
            } else if (la.kind == 11) {
                Get();
            } else if (la.kind == 12) {
                Get();
            } else SynErr(30);
            tt=t.val; 
            Expect(13);
        }
        inst.Operands.Add(VOperand.FromLabel(lb,TypeDec(tt),p)); 
    }

    void Operand(Instruction inst) {
        if (StartOf(1)) {
            MOperand(inst);
        } else if (la.kind == 14 || la.kind == 15) {
            LOperand(inst);
        } else SynErr(31);
    }

    void Instruction() {
        int op=0; string inx; 
        while (!(la.kind == 0 || la.kind == 4)) {SynErr(32); Get();}
        Expect(4);
        inx=t.val; 
        while (la.kind == 5) {
            Get();
            inx+=t.val; 
        }
        op = InstrDec(inx); 
        if(op<0) SemErr("Invalid instruction: "+t.val); 
        var inst = new Instruction(op); 
        if (la.kind == 16) {
            Get();
            while (StartOf(2)) {
                Operand(inst);
            }
            Expect(17);
        }
        instructions.Add(inst); instr_idx++; 
    }

    void LabelDef() {
        string lb; 
        while (!(la.kind == 0 || la.kind == 18)) {SynErr(33); Get();}
        Expect(18);
        Expect(4);
        lb=t.val; 
        Expect(18);
        if(labels.ContainsKey(lb))
         SemErr("Duplicate label \""+lb+"\"");
        labels[lb] = new Label(lb,instr_idx); 
    }

    void DataPayload(DataInstruction dat) {
        int? val=0; 
        if (la.kind == 3) {
            Get();
            dat.Append(EscapeChars(t.val)); 
        } else if (la.kind == 1 || la.kind == 2) {
            Constant(ref val);
            dat.Append(val.Value); 
        } else SynErr(34);
    }

    void DataInstruction() {
        var dat = new DataInstruction(); 
        while (!(la.kind == 0 || la.kind == 19)) {SynErr(35); Get();}
        Expect(19);
        Expect(16);
        DataPayload(dat);
        while (WeakSeparator(20,3,4) ) {
            DataPayload(dat);
        }
        Expect(17);
        instructions.Add(dat); instr_idx++; 
    }

    void RDataInstruction() {
        var dat = new DataInstruction(); int? val=0, rep=0; 
        while (!(la.kind == 0 || la.kind == 21)) {SynErr(36); Get();}
        Expect(21);
        Expect(16);
        Constant(ref val);
        ExpectWeak(20, 5);
        Constant(ref rep);
        Expect(17);
        for(int i = 0; i < rep.Value; i++) dat.Append(val.Value);
        instructions.Add(dat); instr_idx++; 
    }

    void OffsetInstruction() {
        int? offset=0; 
        while (!(la.kind == 0 || la.kind == 22)) {SynErr(37); Get();}
        Expect(22);
        Expect(16);
        Constant(ref offset);
        Expect(17);
        var dat = new OffsetInstruction(offset.Value);
        instructions.Add(dat); instr_idx++; 
    }

    void FileInstruction() {
        string fn; 
        while (!(la.kind == 0 || la.kind == 23)) {SynErr(38); Get();}
        Expect(23);
        Expect(16);
        Expect(3);
        fn = t.val.Trim('"'); 
        Expect(17);
        var dat = new FileInstruction(fn);
        instructions.Add(dat); instr_idx++; 
    }

    void CompilerOptions() {
        if (la.kind == 19) {
            DataInstruction();
        } else if (la.kind == 21) {
            RDataInstruction();
        } else if (la.kind == 22) {
            OffsetInstruction();
        } else if (la.kind == 23) {
            FileInstruction();
        } else SynErr(39);
    }

    void Code() {
        Expect(24);
        while (StartOf(6)) {
            if (la.kind == 4) {
                Instruction();
            } else if (la.kind == 18) {
                LabelDef();
            } else {
                CompilerOptions();
            }
        }
    }



    public void Parse() {
        la = new Token();
        la.val = "";		
        Get();
        Code();
        Expect(0);

    }
    
    static readonly bool[,] set = {
        {T,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,T,T,T, x,x,x},
        {x,T,T,x, x,x,T,T, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x},
        {x,T,T,x, x,x,T,T, T,x,x,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x},
        {x,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x},
        {x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x},
        {T,T,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,T,T,T, x,x,x},
        {x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,T,T,T, x,x,x}

    };
} // end Parser


public class Errors {
    public int count = 0;                                    // number of errors detected
    public System.IO.TextWriter errorStream = Console.Out;   // error messages go to this stream
    public string errMsgFormat = "-- line {0} col {1}: {2}"; // 0=line, 1=column, 2=text

    public virtual void SynErr (int line, int col, int n) {
        string s;
        switch (n) {
            case 0: s = "EOF expected"; break;
            case 1: s = "dec_number expected"; break;
            case 2: s = "hex_number expected"; break;
            case 3: s = "string expected"; break;
            case 4: s = "ident expected"; break;
            case 5: s = "subident expected"; break;
            case 6: s = "\"s\" expected"; break;
            case 7: s = "\"*\" expected"; break;
            case 8: s = "\"$\" expected"; break;
            case 9: s = "\"(\" expected"; break;
            case 10: s = "\"b\" expected"; break;
            case 11: s = "\"h\" expected"; break;
            case 12: s = "\"w\" expected"; break;
            case 13: s = "\")\" expected"; break;
            case 14: s = "\"%\" expected"; break;
            case 15: s = "\"&\" expected"; break;
            case 16: s = "\"[\" expected"; break;
            case 17: s = "\"]\" expected"; break;
            case 18: s = "\"|\" expected"; break;
            case 19: s = "\"!db\" expected"; break;
            case 20: s = "\",\" expected"; break;
            case 21: s = "\"!rep\" expected"; break;
            case 22: s = "\"!org\" expected"; break;
            case 23: s = "\"!fi\" expected"; break;
            case 24: s = "\"@program\" expected"; break;
            case 25: s = "??? expected"; break;
            case 26: s = "invalid Constant"; break;
            case 27: s = "invalid MOperand"; break;
            case 28: s = "invalid MOperand"; break;
            case 29: s = "invalid LOperand"; break;
            case 30: s = "invalid LOperand"; break;
            case 31: s = "invalid Operand"; break;
            case 32: s = "this symbol not expected in Instruction"; break;
            case 33: s = "this symbol not expected in LabelDef"; break;
            case 34: s = "invalid DataPayload"; break;
            case 35: s = "this symbol not expected in DataInstruction"; break;
            case 36: s = "this symbol not expected in RDataInstruction"; break;
            case 37: s = "this symbol not expected in OffsetInstruction"; break;
            case 38: s = "this symbol not expected in FileInstruction"; break;
            case 39: s = "invalid CompilerOptions"; break;

            default: s = "error " + n; break;
        }
        errorStream.WriteLine(errMsgFormat, line, col, s);
        count++;
    }

    public virtual void SemErr (int line, int col, string s) {
        errorStream.WriteLine(errMsgFormat, line, col, s);
        count++;
    }
    
    public virtual void SemErr (string s) {
        errorStream.WriteLine(s);
        count++;
    }
    
    public virtual void Warning (int line, int col, string s) {
        errorStream.WriteLine(errMsgFormat, line, col, s);
    }
    
    public virtual void Warning(string s) {
        errorStream.WriteLine(s);
    }
} // Errors


public class FatalError: Exception {
    public FatalError(string m): base(m) {}
}
}