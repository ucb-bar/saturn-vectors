package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import saturn.common._
import saturn.backend._
import hardfloat._
import saturn.common.FloatCasts._
import saturn.common.FloatCasts.{FP64U => FP64, FP32U => FP32}

/*object FP64 {
    val F = FType.D
    def IEEE2REC(x: UInt) = F.recode(x) // ieee -> recode
    def REC2IEEE(x: UInt) = F.ieee(x) // recode -> ieee
    def one_recoded: UInt = F.recode("h3FF0000000000000".U(64.W)) // recoded 1
}

object FP32 {
    val F = FType.S
    def IEEE2REC(x: UInt) = F.recode(x) // ieee -> recode
    def REC2IEEE(x: UInt) = F.ieee(x) // recode -> ieee
    def one_recoded: UInt = F.recode("h3F800000".U(32.W)) // recoded 1
}

// FP32에 대해서 FP
class RecCast(eIn: Int, sIn: Int, eOut: Int, sOut: Int) extends Module {
    val io = IO(new Bundle{
        val in = Input(UInt((1 + eIn + sIn).W))
        val out = Output(UInt((1 + eOut + sOut).W))
    })
    val c = Module(new RecFNToRecFN(eIn, sIn, eOut, sOut))
    c.io.in := io.in
    c.io.roundingMode := 0.U
    c.io.detectTininess := hardfloat.consts.tininess_afterRounding
    io.out := c.io.out
}*/




// a*b+c
// op=1이면 a*b-c, op=0이면 a*b+c
class RecAdd(latency: Int = 1)(implicit p: Parameters) extends Module {
    val io = IO(new Bundle{
        val valid = Input(Bool())
        val a_recoded = Input(UInt(65.W))
        val b_recoded = Input(UInt(65.W))
        val is_sub = Input(Bool())
        val out = Output(UInt(65.W))
    })
    val fma = Module(new MulAddRecFNPipe(latency, FP64.F.exp, FP64.F.sig))
    fma.io.validin := io.valid
    fma.io.op := Mux(io.is_sub, "b01".U, "b00".U) // 0: add, 1: sub
    fma.io.roundingMode := 0.U
    fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
    fma.io.a := io.a_recoded
    fma.io.b := FP64.one_recoded
    fma.io.c := io.b_recoded // b_recoded가 c로 들어감
    io.out := fma.io.out // ieee로 안하는 이유가 바로 다음 연산에서 recoded 형태로 쓰기 위해서
}

class RecMul(latency: Int = 1)(implicit p: Parameters) extends Module {
    val io = IO(new Bundle{
        val valid = Input(Bool())
        val a_recoded = Input(UInt(65.W))
        val b_recoded = Input(UInt(65.W))
        val is_sub = Input(Bool())
        val out = Output(UInt(65.W))
    })
    val fma = Module(new MulAddRecFNPipe(latency, FP64.F.exp, FP64.F.sig))
    fma.io.validin := io.valid
    fma.io.op := "b00".U
    fma.io.roundingMode := 0.U
    fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
    fma.io.a := io.a_recoded
    fma.io.b := io.b_recoded
    fma.io.c := 0.U
    io.out := fma.io.out
}

class CustomExecuteMicroOpWithData(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
    val data = UInt(dLen.W)
    val eg = UInt(log2Ceil(egsTotal).W)
    val eidx = UInt(log2Ceil(maxVLMax).W)
    val tail = Bool()
    val vat = UInt(vParams.vatSz.W)
    val dt = UInt(64.W)
    val dt_inv = UInt(64.W)
    val s1_error = UInt(65.W)
    val isF32 = Bool()
}

class CustomExecuteMicroOpWithData2(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
    val data2 = UInt(dLen.W)
    val data = UInt(dLen.W)
    val eg = UInt(log2Ceil(egsTotal).W)
    val eidx = UInt(log2Ceil(maxVLMax).W)
    val tail = Bool()
    val vat = UInt(vParams.vatSz.W)
    val dt = UInt(64.W)
    val dt_inv = UInt(64.W)
    val s1_error = UInt(65.W)
    val isF32 = Bool()
}

class CustomExecuteMicroOpWithData3(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
    val base = new CustomExecuteMicroOpWithData2
    val s3_P = UInt(65.W)
    val s3_I = UInt(65.W)
    val s3_D = UInt(65.W)
    val isF32 = Bool()
}

class CustomExecuteMicroOpWithData4(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
    val eg = UInt(log2Ceil(egsTotal).W)
    val vat = UInt(vParams.vatSz.W)
    val prev_error = UInt(65.W)
    val prev_I = UInt(65.W)
    val P = UInt(65.W)
    val I = UInt(65.W)
    val D = UInt(65.W)
    val isF32 = Bool()
}

class CustomExecuteMicroOpWithData5(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
    val eg = UInt(log2Ceil(egsTotal).W)
    val vat = UInt(vParams.vatSz.W)
    val prev_error = UInt(65.W)
    val prev_I = UInt(65.W)
    val out = UInt(65.W)
    val isF32 = Bool()
}

class CustomExecuteMicroOpWithData6(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
    val eg = UInt(log2Ceil(egsTotal).W)
    val vat = UInt(vParams.vatSz.W)
    val prev_error = UInt(65.W)
    val prev_I = UInt(65.W)
    val out = UInt(65.W)
    val isF32 = Bool()
}


// @@@@ pipeDepth는 hazard 슬롯 개수. S0 pre-stage 추가로 파이프가 1단 깊어짐.
//      슬롯 매핑: 0=issue(s0q.enq, 콤비), 1=s0q.deq, 2=s1q.deq, 3..8=s2q..s7q.deq(8=write 사이클).
//      따라서 슬롯 9개(0..8). slot0만 콤비(issue)→Backend가 older_writes에서 drop(1).
class CustomExecutionUnit(pipeDepth: Int = 9)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
    val io = IO(new Bundle{
        // seq->exu
        val iss = Flipped(Decoupled(new CustomExecuteMicroOp))
        // vrf->exu
        val rvs2_data = Input(UInt(dLen.W))
        // exu->vrf
        val pipe_write = Output(Valid(new VectorWrite(dLen)))
        
        val pipe_hazards = Output(Vec(pipeDepth, Valid(new PipeHazard(pipeDepth))))
        val busy = Output(Bool())
    })
    // helper
/*    def laneIEEE(x: UInt, i: Int) = x(64 * (i+1) - 1, 64 * i)
    def laneRec(x: UInt, i: Int) = FP64.IEEE2REC(laneIEEE(x, i))
    def laneIEEE32(x: UInt, i: Int) = x(32 * (i+1) - 1, 32 * i)
    def laneF32_to_recD(x: UInt, i: Int): UInt = {
      val recS = FP32.IEEE2REC(laneIEEE32(x, i))
      val cast = Module(new RecCast(FP32.F.exp,FP32.F.sig,FP64.F.exp,FP64.F.sig))
      cast.io.in := recS
      cast.io.out
    }
    def recD_to_IEEE32(x: UInt): UInt = {
      val cast = Module(new RecCast(FP64.F.exp,FP64.F.sig,FP32.F.exp,FP32.F.sig))
      cast.io.in := x
      FP32.REC2IEEE(cast.io.out)
    }
*/
    
    // 파이프라인 진행 스테이지 추적 (inflight 개수)
    val inFlight = RegInit(0.U(log2Ceil(pipeDepth + 1).W))
    // 결과 valid 시프트 (고정 지연 pipeDepth)
    val vPipe = RegInit(VecInit(Seq.fill(pipeDepth)(false.B)))
    // data
    val dataPipe = Reg(Vec(pipeDepth, UInt(dLen.W)))
    // 입력 수락 조건: 파이프라인에 빈 슬롯 있으면 ok
    //io.iss.ready := inFlight < pipeDepth.U

    // Hazard helper
    private def setHazard(slot: Int, v: Bool, eg: UInt, vat: UInt): Unit = {
        io.pipe_hazards(slot).valid := v
        io.pipe_hazards(slot).bits.eg := eg
        // @@@@ 1.13.0 PipeHazard has `latency`(default 0) instead of `vat`; vat unused for custom write port
    }
    // default
    for (i <-0 until pipeDepth){
        io.pipe_hazards(i).valid := false.B
        io.pipe_hazards(i).bits := 0.U.asTypeOf(io.pipe_hazards(i).bits)
    }


    // S0
    when(io.iss.fire){
        vPipe(0) := true.B // 새 valid 입력
        dataPipe(0) := io.rvs2_data // 새 데이터 입력
        inFlight := inFlight + 1.U
    }.otherwise {
        vPipe(0) := false.B
    }
    for (i <- 1 until pipeDepth) {
        vPipe(i) := vPipe(i-1)  // valid 시프트가 없음
        dataPipe(i) := dataPipe(i-1)  // 데이터 시프트
    }

    val isEgEven = io.iss.bits.eg(0) === 0.U // eg가 짝수인지 여부
    
    /*input_data.data := io.rvs2_data // rvs2 값이 iss와 같이 오기를 바라야함
    input_data.eg := io.iss.bits.eg
    input_data.eidx := io.iss.bits.eidx
    input_data.tail := io.iss.bits.tail
    input_data.vat := io.iss.bits.vat
    input_data.dt := io.iss.bits.dt
    input_data.dt_inv := io.iss.bits.dt_inv
    // 중간 result
    input_data.s1_error := 0.U*/

    // stage를 queue로 작성해서 실험
    val s1q = Module(new Queue(new CustomExecuteMicroOpWithData, 1))
    val s2q = Module(new Queue(new CustomExecuteMicroOpWithData2, 1))
    val s3q = Module(new Queue(new CustomExecuteMicroOpWithData3, 1))
    val s4q = Module(new Queue(new CustomExecuteMicroOpWithData3, 1))
    val s5q = Module(new Queue(new CustomExecuteMicroOpWithData4, 1))
    val s6q = Module(new Queue(new CustomExecuteMicroOpWithData5, 1))
    val s7q = Module(new Queue(new CustomExecuteMicroOpWithData6, 1))
    val s0q = Module(new Queue(new CustomExecuteMicroOpWithData, 1)) // @@@@ S0 pre-stage: even-eg를 hold하며 curr_error(1) 계산
    //val output_stage = Module(new Queue(new CustomExecuteMicroOpWithData, 1))

    // @@@@ S0: even-eg를 s0q에 hold하고 curr_error를 latency 1로 계산한다.
    //      v4-drop 방지 불변식 유지: error는 (조합 포워딩이 아니라) 결국 s1q에 실려 페어링까지 held.
    //      chipyard1 대비 차이: curr_error 입력을 transient io.rvs2_data가 아니라 s0q.deq(held)에서 읽고,
    //      errReady로 latency-1 결과가 준비된 뒤에만 s1q로 옮긴다 → 토큰 gap/백프레셔에도 만료 없음.
    s0q.io.enq.valid := io.iss.valid && isEgEven
    s0q.io.enq.bits.data := io.rvs2_data
    s0q.io.enq.bits.eg := io.iss.bits.eg
    s0q.io.enq.bits.eidx := io.iss.bits.eidx
    s0q.io.enq.bits.tail := io.iss.bits.tail
    s0q.io.enq.bits.vat := io.iss.bits.vat
    s0q.io.enq.bits.dt := io.iss.bits.dt
    s0q.io.enq.bits.dt_inv := io.iss.bits.dt_inv
    s0q.io.enq.bits.isF32 := io.iss.bits.isF32
    s0q.io.enq.bits.s1_error := 0.U

    // S0 계산: curr_error latency 1, s0q에 held된 even 데이터에서 계산
    val curr_error = Module(new RecAdd(1))
    curr_error.io.valid := s0q.io.deq.valid
    curr_error.io.a_recoded := Mux(s0q.io.deq.bits.isF32, laneF32_to_recD(s0q.io.deq.bits.data, 0), laneRec(s0q.io.deq.bits.data, 0))
    curr_error.io.b_recoded := Mux(s0q.io.deq.bits.isF32, laneF32_to_recD(s0q.io.deq.bits.data, 1), laneRec(s0q.io.deq.bits.data, 1))
    curr_error.io.is_sub := true.B

    // @@@@ errReady: even이 s0q.deq에 ≥1사이클 머물면 curr_error(1) 결과가 준비됨(s1q hold가 latency 흡수)
    val errReady = RegInit(false.B)
    when (s0q.io.deq.fire)       { errReady := false.B }
    .elsewhen (s0q.io.deq.valid) { errReady := true.B  }

    // S1: error 준비되면 {even data + registered error}를 s1q로 옮긴다(이후 hold/페어링은 기존과 동일)
    s1q.io.enq.valid := s0q.io.deq.valid && errReady
    s1q.io.enq.bits := s0q.io.deq.bits
    s1q.io.enq.bits.s1_error := curr_error.io.out // @@@@ registered error를 s1q에 hold
    s0q.io.deq.ready := s1q.io.enq.ready && errReady
    // @@@@ odd eg가 실제로 s2q에 enq될 때만 even 데이터를 빼낸다(페어링과 결합).
    //      구: `:= s2q.io.enq.ready` 는 odd eg가 늦으면 even 데이터를 허공으로 흘려보내 드롭을 유발.
    s1q.io.deq.ready := s2q.io.enq.ready && s2q.io.enq.valid

    
    // S2
    // @@@@ F64: even이 s1q.deq에 도착(held)했을 때만 odd와 페어링. s0 추가로 even이 늦게 오므로 필수.
    s2q.io.enq.valid := Mux(s1q.io.deq.bits.isF32, s1q.io.deq.valid, (io.iss.valid && !isEgEven && s1q.io.deq.valid)) // iss가 valid하고 eg가 홀수이며 even이 s1q에 있을 때만 enq
    s2q.io.enq.bits.data2 := Mux(s1q.io.deq.bits.isF32, s1q.io.deq.bits.data, io.rvs2_data)
    s2q.io.enq.bits.data := s1q.io.deq.bits.data
    s2q.io.enq.bits.eg := Mux(s1q.io.deq.bits.isF32, s1q.io.deq.bits.eg, io.iss.bits.eg)
    s2q.io.enq.bits.eidx := Mux(s1q.io.deq.bits.isF32, s1q.io.deq.bits.eidx, io.iss.bits.eidx)
    s2q.io.enq.bits.tail := Mux(s1q.io.deq.bits.isF32, s1q.io.deq.bits.tail, io.iss.bits.tail)
    s2q.io.enq.bits.vat := Mux(s1q.io.deq.bits.isF32, s1q.io.deq.bits.vat, io.iss.bits.vat)
    s2q.io.enq.bits.dt := s1q.io.deq.bits.dt
    s2q.io.enq.bits.dt_inv := s1q.io.deq.bits.dt_inv
    s2q.io.enq.bits.s1_error := s1q.io.deq.bits.s1_error // @@@@ 저장된 even-eg error 사용
    s2q.io.enq.bits.isF32 := s1q.io.deq.bits.isF32
    s2q.io.deq.ready := s3q.io.enq.ready
    
    // @@@@ even: s0q 준비. odd(F64): even이 s1q에 있고 s2q 준비됐을 때만 수락(미리 fire→드롭 방지). F32 odd: 그대로.
    io.iss.ready := Mux(isEgEven, s0q.io.enq.ready, Mux(io.iss.bits.isF32, true.B, s2q.io.enq.ready && s1q.io.deq.valid)) // eg가 짝수일 때 s0 큐, 홀수일 때 s2 큐+even hold 확인

    // S2 계산
    val Kp_times_error = Module(new RecMul(1))
    Kp_times_error.io.valid := s2q.io.enq.valid
    Kp_times_error.io.a_recoded := Mux(s1q.io.deq.bits.isF32, laneF32_to_recD(s1q.io.deq.bits.data,2), laneRec(s1q.io.deq.bits.data,2)) // Kp
    Kp_times_error.io.b_recoded := s1q.io.deq.bits.s1_error // @@@@ 저장된 even-eg error
    Kp_times_error.io.is_sub := false.B

    val error_times_dt = Module(new RecMul(1))
    error_times_dt.io.valid := s2q.io.enq.valid
    error_times_dt.io.a_recoded := s1q.io.deq.bits.s1_error // @@@@ 저장된 even-eg error
    error_times_dt.io.b_recoded := FP64.IEEE2REC(s1q.io.deq.bits.dt) // dt
    error_times_dt.io.is_sub := false.B

    val error_minus_preverr = Module(new RecAdd(1))
    error_minus_preverr.io.valid := s2q.io.enq.valid
    error_minus_preverr.io.a_recoded := s1q.io.deq.bits.s1_error // @@@@ 저장된 even-eg error
    error_minus_preverr.io.b_recoded := Mux(s1q.io.deq.bits.isF32, laneF32_to_recD(s2q.io.enq.bits.data2,6), laneRec(io.rvs2_data,2)) // preverr (F32: lane 6)
    error_minus_preverr.io.is_sub := true.B

    // S3
    s3q.io.enq.valid := s2q.io.deq.valid
    s3q.io.enq.bits.base := s2q.io.deq.bits
    s3q.io.enq.bits.s3_P := Kp_times_error.io.out
    s3q.io.enq.bits.s3_I := error_times_dt.io.out
    s3q.io.enq.bits.s3_D := error_minus_preverr.io.out
    s3q.io.enq.bits.isF32 := s2q.io.deq.bits.isF32
    s3q.io.deq.ready := s4q.io.enq.ready

    //s3 계산
    val I_plus_prevI = Module(new RecAdd(1))
    I_plus_prevI.io.valid := s3q.io.enq.valid
    I_plus_prevI.io.a_recoded := error_times_dt.io.out
    I_plus_prevI.io.b_recoded := Mux(s2q.io.deq.bits.isF32, laneF32_to_recD(s2q.io.deq.bits.data2,5), laneRec(s2q.io.deq.bits.data2,1)) // prevI (F32: lane 5)
    I_plus_prevI.io.is_sub := false.B

    val D_div_dt = Module(new RecMul(1))
    D_div_dt.io.valid := s3q.io.enq.valid
    D_div_dt.io.a_recoded := error_minus_preverr.io.out
    D_div_dt.io.b_recoded := FP64.IEEE2REC(s2q.io.deq.bits.dt_inv) // dt_inv
    D_div_dt.io.is_sub := false.B

    // S4
    s4q.io.enq.valid := s3q.io.deq.valid
    s4q.io.enq.bits.base := s3q.io.deq.bits.base
    s4q.io.enq.bits.s3_P := s3q.io.deq.bits.s3_P
    s4q.io.enq.bits.s3_I := I_plus_prevI.io.out
    s4q.io.enq.bits.s3_D := D_div_dt.io.out
    s4q.io.enq.bits.isF32 := s3q.io.deq.bits.isF32
    s4q.io.deq.ready := s5q.io.enq.ready

    // S4 계산
    val I_times_Ki = Module(new RecMul(1))
    I_times_Ki.io.valid := s4q.io.enq.valid
    I_times_Ki.io.a_recoded := I_plus_prevI.io.out
    I_times_Ki.io.b_recoded := Mux(s3q.io.deq.bits.isF32, laneF32_to_recD(s2q.io.deq.bits.data,3), laneRec(s2q.io.deq.bits.data,3)) // Ki
    I_times_Ki.io.is_sub := false.B

    val D_times_Kd = Module(new RecMul(1))
    D_times_Kd.io.valid := s4q.io.enq.valid
    D_times_Kd.io.a_recoded := D_div_dt.io.out
    D_times_Kd.io.b_recoded := Mux(s3q.io.deq.bits.isF32, laneF32_to_recD(s2q.io.deq.bits.data2,4), laneRec(s2q.io.deq.bits.data2,0)) // Kd (F32: lane 4)
    D_times_Kd.io.is_sub := false.B

    // S5
    s5q.io.enq.valid := s4q.io.deq.valid
    s5q.io.enq.bits.eg := s4q.io.deq.bits.base.eg
    s5q.io.enq.bits.vat := s4q.io.deq.bits.base.vat
    s5q.io.enq.bits.prev_error := s4q.io.deq.bits.base.s1_error
    s5q.io.enq.bits.prev_I := s4q.io.deq.bits.s3_I
    s5q.io.enq.bits.P := s4q.io.deq.bits.s3_P
    s5q.io.enq.bits.I := I_times_Ki.io.out
    s5q.io.enq.bits.D := D_times_Kd.io.out
    s5q.io.enq.bits.isF32 := s4q.io.deq.bits.isF32
    s5q.io.deq.ready := s6q.io.enq.ready

    // S5 계산
    val P_plus_I = Module(new RecAdd(1))
    P_plus_I.io.valid := s5q.io.enq.valid
    P_plus_I.io.a_recoded := s4q.io.deq.bits.s3_P
    P_plus_I.io.b_recoded := I_times_Ki.io.out
    P_plus_I.io.is_sub := false.B

    // S6
    s6q.io.enq.valid := s5q.io.deq.valid
    s6q.io.enq.bits.eg := s5q.io.deq.bits.eg
    s6q.io.enq.bits.vat := s5q.io.deq.bits.vat
    s6q.io.enq.bits.prev_error := s5q.io.deq.bits.prev_error
    s6q.io.enq.bits.prev_I := s5q.io.deq.bits.prev_I
    s6q.io.enq.bits.out := P_plus_I.io.out
    s6q.io.enq.bits.isF32 := s5q.io.deq.bits.isF32
    s6q.io.deq.ready := s7q.io.enq.ready

    // S6 계산
    val final_out = Module(new RecAdd(1))
    final_out.io.valid := s6q.io.enq.valid
    final_out.io.a_recoded := P_plus_I.io.out
    final_out.io.b_recoded := s5q.io.deq.bits.D
    final_out.io.is_sub := false.B
    // --- isF32 파이프(발사시점의 모드 플래그를 S7까지 동기화) ---
    /*val isF32Pipe = RegInit(VecInit(Seq.fill(pipeDepth)(false.B)))
    when (io.iss.fire) { isF32Pipe(0) := io.iss.bits.isF32 } .otherwise { isF32Pipe(0) := false.B }
    for (i <- 1 until pipeDepth) { isF32Pipe(i) := isF32Pipe(i-1) }
    val isF32_wb = isF32Pipe(6)  // S7(writeback) 시점의 모드
*/


    // S7 (REC2IEEE)
    s7q.io.enq.valid := s6q.io.deq.valid
    s7q.io.enq.bits.eg := s6q.io.deq.bits.eg
    s7q.io.enq.bits.vat := s6q.io.deq.bits.vat
    s7q.io.enq.bits.prev_I := s6q.io.deq.bits.prev_I
    s7q.io.enq.bits.prev_error := s6q.io.deq.bits.prev_error
    s7q.io.enq.bits.out := final_out.io.out
    s7q.io.enq.bits.isF32 := s6q.io.deq.bits.isF32
    s7q.io.deq.ready := true.B

    // 64비트 IEEE (항상 생성)
    val prevI64   = FP64.REC2IEEE(s7q.io.deq.bits.prev_I)
    val prevErr64 = FP64.REC2IEEE(s7q.io.deq.bits.prev_error)
    val out64     = FP64.REC2IEEE(s7q.io.deq.bits.out)
    // 32비트 IEEE (F32 모드에서 사용)
    val prevI32   = recD_to_IEEE32(s7q.io.deq.bits.prev_I)
    val prevErr32 = recD_to_IEEE32(s7q.io.deq.bits.prev_error)
    val out32     = recD_to_IEEE32(s7q.io.deq.bits.out)

    // 각 단계의 eg, vat 전달
    // 각 단계의 eg / vat 전달 (현재 구조 기준)
    setHazard(0, s0q.io.enq.fire,              io.iss.bits.eg,     io.iss.bits.vat)                 // @@@@ issue 사이클(콤비, Backend가 drop(1)로 older_writes에서 제외 → 루프 차단)
    setHazard(1, s0q.io.deq.valid,             s0q.io.deq.bits.eg, s0q.io.deq.bits.vat)             // @@@@ even이 s0q에 held(신규 스테이지)
    setHazard(2, s1q.io.deq.valid,             s1q.io.deq.bits.eg, s1q.io.deq.bits.vat)
    setHazard(3, s2q.io.deq.valid,             s2q.io.deq.bits.eg, s2q.io.deq.bits.vat)
    setHazard(4, s3q.io.deq.valid,             s3q.io.deq.bits.base.eg, s3q.io.deq.bits.base.vat)
    setHazard(5, s4q.io.deq.valid,             s4q.io.deq.bits.base.eg, s4q.io.deq.bits.base.vat)
    setHazard(6, s5q.io.deq.valid,             s5q.io.deq.bits.eg, s5q.io.deq.bits.vat)
    setHazard(7, s6q.io.deq.valid,             s6q.io.deq.bits.eg, s6q.io.deq.bits.vat)
    setHazard(8, s7q.io.deq.valid,             s7q.io.deq.bits.eg, s7q.io.deq.bits.vat)  // @@@@ write 사이클 커버

    io.busy := s0q.io.deq.valid || s1q.io.deq.valid || s2q.io.deq.valid || s3q.io.deq.valid ||
                s4q.io.deq.valid || s5q.io.deq.valid || s6q.io.deq.valid || s7q.io.deq.valid

    // Output stage
    /*output_stage.io.enq.valid := s1q.io.deq.valid
    output_stage.io.enq.bits := s1q.io.deq.bits
    output_stage.io.enq.bits.s1_error := curr_error.io.out
    output_stage.io.deq.ready := true.B*/
    // 64b: [out64][prevErr64][prevI64][dummy64]
    // 32b: 상단 96b만 사용 [out32][prevErr32][prevI32] + 32b 패딩, 나머지 160b는 무시
    val wbData64 = Cat(out64,     prevErr64, prevI64,  ((~0.U(64.W))))
    val wbData32 = Cat(out32,     prevErr32, prevI32,  0.U(160.W))

    io.pipe_write.bits.data := Mux(s7q.io.deq.bits.isF32, wbData32, wbData64)


    io.pipe_write.bits.mask := Mux(s7q.io.deq.bits.isF32,
      ((~0.U(256.W)) << 160),  // F32: [255:160]만 write enable
      ((~0.U(256.W)) <<  64)   // F64: [255:64]만 write enable
    )
    io.pipe_write.bits.eg := s7q.io.deq.bits.eg
    // io.pipe_write.bits.mask := ((~0.U(256.W)) << 64) // 256비트 전체 1을 만든 뒤 하위 64비트를 0으로
    io.pipe_write.valid := s7q.io.deq.valid
}