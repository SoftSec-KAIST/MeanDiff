module MeanDiff.MultProc

open Report

type Msg =
  | BasicResponse
  | InsnResponse of string
  | EndResponse

type ProcessMessage =
  | PM of Msg * AsyncReplyChannel<ProcessMessage>

type Agent = MailboxProcessor<ProcessMessage>

let createAgent insnList =
  MailboxProcessor.Start(fun inbox ->
      let rec loop cnt insnList =
        async {
            let! PM(_, replyChannel) = inbox.Receive()
            match insnList with
            | [] ->
                replyChannel.Reply(PM(EndResponse, replyChannel))
                return! (loop cnt insnList)
            | hd :: tl ->
                replyChannel.Reply(PM(InsnResponse hd, replyChannel))
                if (cnt + 1) % 1000 = 0
                then
                  printfn "%d instructions have been tested" (cnt + 1)
                return! (loop (cnt + 1) tl)
        }
      loop 0 insnList)

let rec worker option (agent : Agent) =
  async {
      let msg =
        match (agent.PostAndReply(fun replyChannel -> PM(BasicResponse, replyChannel))) with
        | PM (x, _) -> x
      match msg with
      | InsnResponse (insn) ->
          do! reporting option insn
          return! (worker option agent)
      | EndResponse -> ()
      | _ -> raise InvalidMessage
  }
