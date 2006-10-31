-- Key Bindings

defbindings("WFrame", {
  kpress(MOD1.."Next", "WFrame.switch_next(_)"),
  kpress(MOD1.."Prior", "WFrame.switch_prev(_)"),

  bdoc("Show/hide tab"),
  mpress(MOD1.."Button1", "WFrame.set_tabbar(_, 'toggle')"),

  bdoc("Quickly rename a frame."),
  mpress(MOD1.."Shift+Button3", "mod_query.query_renameframe(_)"),
})

defbindings("WTiling", {
    kpress(MOD1.."Up", "ioncore.goto_next(_sub, 'up')"),
    kpress(MOD1.."Down", "ioncore.goto_next(_sub, 'down')"),
    kpress(MOD1.."Right", "ioncore.goto_next(_sub, 'right')"),
    kpress(MOD1.."Left", "ioncore.goto_next(_sub, 'left')"),
})


-- Window matching

defwinprop {
  class = "Firefox-bin",
  instance = "Gecko",
--  name = ".* - Mozilla Firefox",
  target = "firefox-frame",
}

defwinprop {
  class = "Emacs",
  instance = "main",
  target = "emacs-default-frame"
}

defwinprop {
  class = "URxvt",
  instance = "x-terminal-emulator",
  target = "terminal-frame",
}

defwinprop {
  class = "Emacs",
  instance = "erc",
  target = "irc-frame",
}

defwinprop {
  class = "Gaim",
  instance = "gaim",
  target = "irc-frame",
}

defwinprop {
  class = "trayer",
  instance = "panel",
  target = "systray-frame",
}
