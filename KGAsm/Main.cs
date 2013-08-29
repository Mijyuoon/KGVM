using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.Threading.Tasks;

using MijStuff.VM2;

namespace MijStuff.VM2.Asm {

    public enum OpType {
        Number, Stack, Label
    }

    public class Label {
        public string Name { get; internal set; }
        public int Index { get; internal set; }
        public int Address { get; set; }

        public Label(string Name, int Index) {
            this.Name = Name; this.Index = Index;
        }

        public Label(string Name, int Index, int Address) {
            this.Name = Name; 
            this.Index = Index; 
            this.Address = Address;
        }
    }

    public class Operand {
        public int? Value { get; set; }
        public OpType Type { get; set; }
        public bool Ptr { get; set; }
        public int Size { get; set; }
        public string LbName { get; set; }
        public bool ForceFull { get; set; }

        static public byte[] GetBytesFrom(int number, bool full) {
            if(full) return BitConverter.GetBytes(number);
            if(Byte.MinValue <= number && number <= Byte.MaxValue)
                return new[] { (byte)number };
            if(Int16.MinValue <= number && number <= Int16.MaxValue)
                return BitConverter.GetBytes( (short)number );
            return BitConverter.GetBytes(number);
        }

        public int GetSize() { switch(Type) {
            case OpType.Label: return 5;
            case OpType.Number:
                return GetBytesFrom(Value.Value, ForceFull).Length + 1;
            case OpType.Stack: return 1;
            default: return 0;
        }}

        public List<byte> Emit() {
            var data = new List<byte>();
            var byte1 = Ptr ? (byte)64 : (byte)0;
            switch(Type) {
                case OpType.Label:
                case OpType.Number:
                    if(Type == OpType.Label && Value == null)
                        throw new Exception("Unresolved label \"" + LbName + "\"");
                    byte1 |= (byte)(Size >> 1); byte[] pload=null;
                    if(Type != OpType.Label) {
                        pload = GetBytesFrom(Value.Value, ForceFull);
                        byte1 |= (byte)(pload.Length << 3);
                    } else {
                        pload = BitConverter.GetBytes(Value.Value);
                        byte1 |= (byte)(4 << 3);
                    } data.Add(byte1); data.AddRange(pload);
                break;
                case OpType.Stack:
                    byte1 |= (byte)(Size >> 1); byte1 |= (byte)4;
                    data.Add(byte1);
                break;
            } return data;
        }

        public static Operand FromNumber(int Value, int Size, bool Ptr, bool Full = false) {
            return new Operand {Value = Value, Size = Size, Ptr = Ptr, Type = OpType.Number, ForceFull = Full};
        }
        public static Operand FromStack(int Size, bool Ptr) {
            return new Operand {Value = null, Size = Size, Ptr = Ptr, Type = OpType.Stack};
        }
        public static Operand FromLabel(string Name, int Size, bool Ptr) {
            return new Operand {LbName = Name, Value = null, Size = Size, Ptr = Ptr, Type = OpType.Label};
        }
    }

    public class Instruction {
        public byte OpCode { get; set; }
        public List<Operand> Operands { get; internal set; }

        public Instruction(int opcode) {
            this.OpCode = (byte)(opcode & 0xFF);
            this.Operands = new List<Operand>();
        }
        public virtual int GetCount(int useless) {
            if(Operands.Count > 15)
                throw new Exception("Operand count must be less than 16");
            int size = 2;
            foreach(var op in Operands)
                size += op.GetSize();
            return size;
        }
        public virtual List<byte> Emit() {
            var data = new List<byte>();
            var byte2 = (byte)(Operands.Count << 4);
            data.Add(OpCode); data.Add(byte2);
            foreach(var op in Operands)
                data.AddRange(op.Emit());
            return data;
        }
    }

    public class DataInstruction : Instruction {
        public List<byte> Data { get; internal set; }

        public DataInstruction() : base(0) {
            this.Data = new List<byte>();
        }
        public override int GetCount(int useless) {
            return Data.Count;
        }
        public override List<byte> Emit() {
            return Data;
        }
        public void Append(int data) {
            foreach(var bt in Operand.GetBytesFrom(data, false)) Data.Add(bt);
        }
        public void Append(string data) {
            if(data[0] == '"')
                data = data.Substring(1);
            if(data[data.Length-1] == '"') 
                data = data.Substring(0,data.Length-1);
            Data.AddRange(Encoding.ASCII.GetBytes(data));
        }
    }

    public class OffsetInstruction : Instruction {
        private int Offset;
        private int NumBytes;

        public OffsetInstruction(int offset) : base(0) {
            this.Offset = offset;
        }
        public override int GetCount(int pos) {
            NumBytes = Offset - pos;
            return NumBytes;
        }
        public override List<byte> Emit() {
            return new List<byte>( new byte[NumBytes] );
        }
    }

    public class FileInstruction : Instruction {
        public string FileName;
        public FileStream SFile;

        public FileInstruction(string fname) : base(0) {
            this.FileName = fname;
            this.SFile = File.Open(fname, FileMode.Open);
        }
        public override int GetCount(int useless) {
            return (int)SFile.Length;
        }
        public override List<byte> Emit() {
            var data = new List<byte>();
            for(int i = 0; i < SFile.Length; i++)
                data.Add((byte)SFile.ReadByte());
            SFile.Close(); return data;
        }
    }

    public class KGAsm {
        private Scanner scanner;
        private Parser parser;
        private bool isParsed;

        public int OffsetBase { get; set; }

        public int ErrNum { 
            get { return parser.errors.count; } 
        }

        private void proclabel(Instruction ix) {
            foreach(var opx in ix.Operands) {
                if(opx.Type == OpType.Label && parser.labels.ContainsKey(opx.LbName))
                    opx.Value = parser.labels[opx.LbName].Address;
            }
        }

        public KGAsm(Stream stream) {
            scanner = new Scanner(stream);
            parser = new Parser(scanner);
            parser.instructions = new List<Instruction>();
            parser.labels = new Dictionary<string, Label>();
        }

        public KGAsm(string fname) {
            scanner = new Scanner(fname);
            parser = new Parser(scanner);
            parser.instructions = new List<Instruction>();
            parser.labels = new Dictionary<string, Label>();
        }

        public byte[] Assemble() {
            parser.Parse(); if(parser.errors.count > 0) 
                return new byte[0];
            int offset = OffsetBase; 
            var bytes = new List<byte>();
            for(int idx = 0; idx < parser.instructions.Count; idx++) {
                var ix = parser.instructions[idx];
                foreach(var lb in parser.labels.Values)
                    if(idx == lb.Index) lb.Address = offset;
                var count = ix.GetCount(offset); offset += count;
            } foreach(var ix in parser.instructions) {
                proclabel(ix);
                bytes.AddRange(ix.Emit());
            } isParsed = true;
            return bytes.ToArray();
        }

        public string[] GetLabels() {
            if(parser.errors.count > 0 || !isParsed)
                return new string[0];
            var lst = new List<string>();
            foreach ( var lb in parser.labels ) {
                lst.Add(String.Format("|{0}| {1}", lb.Key, lb.Value.Address));
            }
            return lst.ToArray();
        }

        public void LoadLabels(string[] data) {
            var RE = new Regex(@"^\|(\w+)\|\s+(\d+)$");
            for ( int i = 0; i < data.Length; i++ ) {
                var Mx = RE.Match(data[i].Trim());
                if(!Mx.Success) continue;
                string name = Mx.Groups[1].Value;
                int addr = Convert.ToInt32(Mx.Groups[2].Value);
                parser.labels[name] = new Label(name, -1, addr);
            }
        }
    }
}
