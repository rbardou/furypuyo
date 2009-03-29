let initial_resend_delay = 10
let resend_delay_rate = 2.

type 'a info = {
  msg: 'a;
  mutable last: Time.t;
  mutable delay: Time.d;
}

type 'a sender = {
  frame: 'a Frame.frame;
  buffer: (int, 'a info) Hashtbl.t;
}

let start frame =
  {
    frame = frame;
    buffer = Hashtbl.create 17;
  }

let send sender msg =
  let id = Frame.next sender.frame in
  let info =
    {
      msg = msg;
      last = Time.now ();
      delay = Time.ms initial_resend_delay;
    }
  in
  Hashtbl.add sender.buffer id info;
  let ack () = Hashtbl.remove sender.buffer id in
  Frame.send ~ack sender.frame msg

let check_resend frame id info =
  let now = Time.now () in
  if now >= Time.shift info.last info.delay then begin
    info.last <- now;
    info.delay <- Time.multf info.delay resend_delay_rate;
    Frame.resend frame id info.msg
  end

let update sender =
  Hashtbl.iter (check_resend sender.frame) sender.buffer
