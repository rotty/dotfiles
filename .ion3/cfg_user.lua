-- Key Bindings

defbindings("WFrame", {
  kpress(MOD1.."Next", "WFrame.switch_next(_)"),
  kpress(MOD1.."Prior", "WFrame.switch_prev(_)"),
})

defbindings("WIonWS", {
    kpress(MOD1.."Up", "WIonWS.goto_dir(_, 'above')"),
    kpress(MOD1.."Down", "WIonWS.goto_dir(_, 'below')"),
    kpress(MOD1.."Right", "WIonWS.goto_dir(_, 'right')"),
    kpress(MOD1.."Left", "WIonWS.goto_dir(_, 'left')"),
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
