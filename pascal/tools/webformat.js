/* webformat.js - Cross-checks web/editor.html's file layer against the Pascal.

   web/editor.html is a single self-contained page with no build step and no
   test framework, so its binary writer has no cover of its own - and the byte
   layout next to writeBinary is a contract with the packed records in
   datafile.pas. Drift there garbles every record silently.

   This harness pulls the model and format half of the page's script out of
   the HTML - everything above the DOM wiring - evaluates it under Node, and
   round-trips a world through all three formats. Run by "make webtest",
   which then feeds the binary it wrote to bin/validate so the Pascal side
   has to agree it is a world.

   Node is a development dependency of the tests only. Nothing in the
   editor, the game or the distribution needs it. */

const fs = require("fs");
const path = require("path");

const root = path.resolve(__dirname, "..", "..");
const html = fs.readFileSync(path.join(root, "web", "editor.html"), "utf8");

// Everything from <script> down to the first line of DOM wiring. The marker
// is the comment that separates the two halves of the file; if it moves,
// this stops with a clear message rather than a confusing ReferenceError.
const scriptStart = html.indexOf("<script>");
const cut = html.indexOf('/* ---- Events ---');
if (scriptStart < 0 || cut < 0) {
  console.error("webformat: cannot find the model half of editor.html.");
  console.error("It ends at the '/* ---- Events ---' comment; update this file if that moved.");
  process.exit(2);
}
const source = html.slice(scriptStart + "<script>".length, cut);

// The model half touches the DOM in a few render helpers we never call, and
// $ is used at load time by none of them. A stub keeps evaluation honest
// without pretending to be a browser.
const stub = `
  const document = { getElementById: () => null, querySelectorAll: () => [],
                     createElement: () => ({ style: {}, appendChild(){}, remove(){} }),
                     addEventListener(){}, body: { appendChild(){} } };
  const window = {};
`;

// setWorld is defined here rather than in the page: the editor has no reason
// to expose its module-level `world`, and the harness has no reason to make
// it change shape for a test.
const ctx = {};
(new Function("exports", stub + source + "\n; Object.assign(exports, {" +
  ["blankWorld", "newRoom", "newObject", "newMob", "newEvent", "newCond", "newAction",
   "writeBinary", "readBinary", "writeText", "readText", "writeBPL", "readBPL",
   "validate", "paraTriggers", "danglingParaRefs", "eventSlots", "eventCount"
  ].join(",") + ", setWorld: w => { world = w; } });"
))(ctx);

let failures = 0, checks = 0;
function check(what, cond) {
  checks++;
  if (cond) console.log("  ok    " + what);
  else { console.log("  FAIL  " + what); failures++; }
}
function checkEq(what, got, want) {
  check(what + " = " + JSON.stringify(want) + " (got " + JSON.stringify(got) + ")",
        got === want);
}

/* A world with two rooms, an object, a mob, a paragraph and two events in
   sparse slots - 2 and 5 - because slot numbers are identity and a gap must
   survive every format. */
function sample() {
  const w = ctx.blankWorld();
  w.title = "Web Format Test";
  w.startRoom = 1;
  w.winRoomID = 2;
  w.winObjectID = 1;
  w.introPara = 1;

  const r1 = ctx.newRoom(1);
  r1.name = "Hall"; r1.desc = "A wide hall."; r1.exits.north = 2; r1.points = 5;
  const r2 = ctx.newRoom(2);
  r2.name = "Vault"; r2.desc = "A cold vault."; r2.exits.south = 1;
  r2.firstVisitPara = 1;
  w.rooms.push(r1, r2);

  const o = ctx.newObject(1);
  o.name = "Key"; o.desc = "A brass key."; o.roomID = 1;
  o.flags = { pickup: true, use: true, open: false, read: false };
  o.useText = "It turns.";
  const o2 = ctx.newObject(2);
  o2.name = "Door"; o2.desc = "A vault door."; o2.roomID = 1;
  o2.flags = { pickup: false, use: false, open: true, read: false };
  w.objects.push(o, o2);

  const m = ctx.newMob(1);
  m.name = "Merchant"; m.desc = "A merchant."; m.roomID = 1;
  m.dialogue = "Wares?";
  w.mobs.push(m);

  w.paragraphs[1] = "The door swings wide.\r\nBeyond it, darkness.";

  const e2 = ctx.newEvent("Unlock the vault");
  e2.trigger = "USEOBJECTON";
  e2.triggerID = 1; e2.triggerID2 = 2;
  e2.oneShot = true; e2.enabled = true;
  e2.conds.push({ type: "FLAGISCLEAR", targetID: 1, value: 0, negate: false });
  e2.conds.push({ type: "COUNTERGREATER", targetID: 2, value: -3, negate: true });
  e2.actions.push({ type: "SHOWMESSAGE", targetID: 0, value: 0,
                    text: "It turns, slowly, and then, all at once." });
  e2.actions.push({ type: "SETFLAG", targetID: 1, value: 0, text: "" });
  e2.actions.push({ type: "SHOWPARAGRAPH", targetID: 1, value: 0, text: "" });
  w.events[2] = e2;

  const e5 = ctx.newEvent("Tick");
  e5.trigger = "TIMER";
  e5.triggerID = 5; e5.triggerID2 = 3;
  e5.oneShot = false; e5.enabled = true;
  e5.actions.push({ type: "ADDCOUNTER", targetID: 2, value: 1, text: "" });
  w.events[5] = e5;

  w.flagNames[1] = "vault_open";
  w.counterNames[2] = "ticks";
  return w;
}

// The page keeps its world in a module-level `world`, so the harness swaps it
// the same way the page's own load path does.
function useWorld(w) { ctx.setWorld(w); }

function compareWorlds(label, a, b) {
  checkEq(label + ": title", b.title, a.title);
  checkEq(label + ": rooms", b.rooms.length, a.rooms.length);
  checkEq(label + ": room 1 desc", b.rooms[0].desc, a.rooms[0].desc);
  checkEq(label + ": room 1 north exit", b.rooms[0].exits.north, a.rooms[0].exits.north);
  checkEq(label + ": room 1 points", b.rooms[0].points, a.rooms[0].points);
  checkEq(label + ": room 2 first visit", b.rooms[1].firstVisitPara, a.rooms[1].firstVisitPara);
  checkEq(label + ": objects", b.objects.length, a.objects.length);
  checkEq(label + ": object 2 open flag", b.objects[1].flags.open, true);
  checkEq(label + ": object use text", b.objects[0].useText, a.objects[0].useText);
  checkEq(label + ": object pickup flag", b.objects[0].flags.pickup, true);
  checkEq(label + ": mobs", b.mobs.length, a.mobs.length);
  checkEq(label + ": mob dialogue", b.mobs[0].dialogue, a.mobs[0].dialogue);

  // Slots are identity: 2 and 5 must come back as 2 and 5, with 1, 3 and 4
  // still empty. Compacting them would repoint every existing save game.
  useWorld(b);
  checkEq(label + ": event slots", ctx.eventSlots().join(","), "2,5");
  checkEq(label + ": event count is the highest slot", ctx.eventCount(), 5);
  useWorld(a);

  const e2 = b.events[2], e5 = b.events[5];
  checkEq(label + ": event 2 name", e2.name, a.events[2].name);
  checkEq(label + ": event 2 trigger", e2.trigger, "USEOBJECTON");
  checkEq(label + ": event 2 trigger IDs", e2.triggerID + "/" + e2.triggerID2, "1/2");
  checkEq(label + ": event 2 one-shot", e2.oneShot, true);
  checkEq(label + ": event 2 enabled", e2.enabled, true);
  checkEq(label + ": event 2 conditions", e2.conds.length, 2);
  checkEq(label + ": condition 2 type", e2.conds[1].type, "COUNTERGREATER");
  // A negative condition value has to survive as a signed 16-bit number
  checkEq(label + ": condition 2 value", e2.conds[1].value, -3);
  checkEq(label + ": condition 2 negate", e2.conds[1].negate, true);
  checkEq(label + ": event 2 actions", e2.actions.length, 3);
  // The message field takes everything after the third comma, so commas in
  // the text must not split it into extra fields
  checkEq(label + ": action 1 text", e2.actions[0].text, a.events[2].actions[0].text);
  checkEq(label + ": action 3 paragraph", e2.actions[2].targetID, 1);

  checkEq(label + ": event 5 trigger", e5.trigger, "TIMER");
  checkEq(label + ": event 5 period", e5.triggerID2, 3);
  // The two booleans that share one byte in the binary and are easy to swap
  checkEq(label + ": event 5 one-shot", e5.oneShot, false);
  checkEq(label + ": event 5 enabled", e5.enabled, true);

  checkEq(label + ": flag name", b.flagNames[1], "vault_open");
  checkEq(label + ": counter name", b.counterNames[2], "ticks");
}

console.log("webformat - web editor file layer");
console.log();

const w = sample();

console.log("Binary v4 round-trip:");
useWorld(w);
const bin = ctx.writeBinary();
const view = new DataView(bin);
checkEq("magic", String.fromCharCode(view.getUint8(0), view.getUint8(1),
                                     view.getUint8(2), view.getUint8(3)), "SORB");
checkEq("version", view.getUint16(4, true), 4);
compareWorlds("binary", w, ctx.readBinary(bin, "test.dat"));

console.log();
console.log("Text round-trip:");
useWorld(w);
compareWorlds("text", w, ctx.readText(ctx.writeText()));

console.log();
console.log("BPL round-trip:");
useWorld(w);
compareWorlds("bpl", w, ctx.readBPL(ctx.writeBPL()));

console.log();
console.log("Validation and the cross-reference:");
useWorld(w);
let issues = ctx.validate();
check("a sound world reports no errors",
      issues.filter(i => i.level === "error").length === 0);

// One spoiled field at a time, each standing for a mistake that is invisible
// at run time.
function spoil(fn) {
  const s = sample();
  fn(s);
  useWorld(s);
  return ctx.validate().map(i => i.what).join(" | ");
}
check("a trigger naming a missing object",
      spoil(s => s.events[2].triggerID = 99).includes("names object 99"));
check("a second ID on a trigger that has none",
      spoil(s => { s.events[2].trigger = "TAKEOBJECT"; }).includes("never fires"));
check("a timer that never comes round",
      spoil(s => { s.events[5].triggerID = 0; s.events[5].triggerID2 = 0; })
        .includes("never fires"));
check("an action showing an empty message",
      spoil(s => s.events[2].actions[0].text = "").includes("empty message"));
check("an action naming an empty paragraph",
      spoil(s => s.events[2].actions[2].targetID = 9).includes("which is empty"));
check("an action enabling an empty slot",
      spoil(s => s.events[2].actions[1] =
        { type: "ENABLEEVENT", targetID: 7, value: 0, text: "" }).includes("empty slot"));
check("an exit action with no such direction",
      spoil(s => s.events[2].actions[1] =
        { type: "LOCKEXIT", targetID: 1, value: 6, text: "" }).includes("not a direction"));
check("a flag number past the end",
      spoil(s => s.events[2].actions[1].targetID = 999).includes("flags run 1 to"));
check("an event that starts disabled with nothing to enable it",
      spoil(s => s.events[5].enabled = false).includes("starts disabled"));

useWorld(w);
check("the cross-reference credits the event action",
      ctx.paraTriggers(1).some(t => t.startsWith("event 2 ")));

const s = sample();
s.events[2].actions[2].targetID = 9;
useWorld(s);
check("dangling references name the event and action",
      ctx.danglingParaRefs().some(d => d.startsWith("Event 2 action 3")));

console.log();
if (failures === 0) {
  console.log("webformat: all " + checks + " checks passed");
} else {
  console.log("webformat: " + failures + " of " + checks + " checks FAILED");
  process.exit(1);
}

// Leave the binary behind for the Pascal side to read, if asked.
if (process.argv[2]) {
  useWorld(w);
  fs.writeFileSync(process.argv[2], Buffer.from(new Uint8Array(ctx.writeBinary())));
  console.log("wrote " + process.argv[2] + " for the Pascal reader");
}
