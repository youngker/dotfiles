Config
  { font = "xft:Lucida Grande:Bold:size=11:bold:antialias=true, Noto Sans CJK KR:size=11:antialias=true",
    additionalFonts = ["xft:Font Awesome:size=11:antialias=true"],
    bgColor = "#2E3440",
    fgColor = "#ECEFF4",
    position = TopSize C 100 25,
    sepChar = "%",
    alignSep = "}{",
    template = " %StdinReader%  }{ %multicpu% %memory% %dynnetwork% %date% ",
    lowerOnStart = True,
    hideOnStart = False,
    allDesktops = True,
    overrideRedirect = True,
    pickBroadest = False,
    persistent = True,
    commands =
      [ Run
          DynNetwork
          [ "--template",
            "<fc=#ebcb8b><fn=1>\xf0ac</fn></fc> <tx>/<rx>",
            "-w",
            "3",
            "-c",
            "0"
          ]
          10,
        Run
          MultiCpu
          [ "--template",
            "<fc=#bf616a><fn=1>\xf108</fn></fc> <total0>",
            "-w",
            "2",
            "-c",
            "0"
          ]
          10,
        Run
          CoreTemp
          [ "--template",
            "Temp: <core0>°C|<core1>°C",
            "--Low",
            "70",
            "--High",
            "80",
            "--low",
            "darkgreen",
            "--normal",
            "darkorange",
            "--high",
            "darkred"
          ]
          50,
        Run
          Memory
          [ "--template",
            "<fc=#d08770><fn=1>\xf2db</fn></fc> <usedratio>"
          ]
          10,
        Run Date "<fc=#a3be8c><fn=1>\xf133</fn></fc> %a, %d %B %H:%M" "date" 100,
        Run StdinReader
      ]
  }
