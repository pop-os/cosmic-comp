use packed_struct::prelude::*;

#[derive(PrimitiveEnum_u16, Clone, Copy, PartialEq, Debug, Default)]
pub enum InputCapability {
    #[default]
    None = 0,
    KeyboardKeyEsc = 1,
    KeyboardKey1 = 2,
    KeyboardKey2 = 3,
    KeyboardKey3 = 4,
    KeyboardKey4 = 5,
    KeyboardKey5 = 6,
    KeyboardKey6 = 7,
    KeyboardKey7 = 8,
    KeyboardKey8 = 9,
    KeyboardKey9 = 10,
    KeyboardKey0 = 11,
    KeyboardKeyMinus = 12,
    KeyboardKeyEqual = 13,
    KeyboardKeyBackspace = 14,
    KeyboardKeyTab = 15,
    KeyboardKeyQ = 16,
    KeyboardKeyW = 17,
    KeyboardKeyE = 18,
    KeyboardKeyR = 19,
    KeyboardKeyT = 20,
    KeyboardKeyY = 21,
    KeyboardKeyU = 22,
    KeyboardKeyI = 23,
    KeyboardKeyO = 24,
    KeyboardKeyP = 25,
    KeyboardKeyLeftBrace = 26,
    KeyboardKeyRightBrace = 27,
    KeyboardKeyEnter = 28,
    KeyboardKeyLeftCtrl = 29,
    KeyboardKeyA = 30,
    KeyboardKeyS = 31,
    KeyboardKeyD = 32,
    KeyboardKeyF = 33,
    KeyboardKeyG = 34,
    KeyboardKeyH = 35,
    KeyboardKeyJ = 36,
    KeyboardKeyK = 37,
    KeyboardKeyL = 38,
    KeyboardKeySemicolon = 39,
    KeyboardKeyApostrophe = 40,
    KeyboardKeyGrave = 41,
    KeyboardKeyLeftShift = 42,
    KeyboardKeyBackslash = 43,
    KeyboardKeyZ = 44,
    KeyboardKeyX = 45,
    KeyboardKeyC = 46,
    KeyboardKeyV = 47,
    KeyboardKeyB = 48,
    KeyboardKeyN = 49,
    KeyboardKeyM = 50,
    KeyboardKeyComma = 51,
    KeyboardKeyDot = 52,
    KeyboardKeySlash = 53,
    KeyboardKeyRightShift = 54,
    KeyboardKeyKpAsterisk = 55,
    KeyboardKeyLeftAlt = 56,
    KeyboardKeySpace = 57,
    KeyboardKeyCapsLock = 58,
    KeyboardKeyF1 = 59,
    KeyboardKeyF2 = 60,
    KeyboardKeyF3 = 61,
    KeyboardKeyF4 = 62,
    KeyboardKeyF5 = 63,
    KeyboardKeyF6 = 64,
    KeyboardKeyF7 = 65,
    KeyboardKeyF8 = 66,
    KeyboardKeyF9 = 67,
    KeyboardKeyF10 = 68,
    KeyboardKeyNumLock = 69,
    KeyboardKeyScrollLock = 70,
    KeyboardKeyKP7 = 71,
    KeyboardKeyKP8 = 72,
    KeyboardKeyKP9 = 73,
    KeyboardKeyKPMinus = 74,
    KeyboardKeyKP4 = 75,
    KeyboardKeyKP5 = 76,
    KeyboardKeyKP6 = 77,
    KeyboardKeyKPPlus = 78,
    KeyboardKeyKP1 = 79,
    KeyboardKeyKP2 = 80,
    KeyboardKeyKP3 = 81,
    KeyboardKeyKP0 = 82,
    KeyboardKeyKPDot = 83,

    KeyboardKeyZenkakuhankaku = 85,
    KeyboardKey102nd = 86,
    KeyboardKeyF11 = 87,
    KeyboardKeyF12 = 88,
    KeyboardKeyRo = 89,
    KeyboardKeyKatakana = 90,
    KeyboardKeyHiragana = 91,
    KeyboardKeyHenkan = 92,
    KeyboardKeyKatakanaHiragana = 93,
    KeyboardKeyMuhenkan = 94,
    KeyboardKeyKPJpComma = 95,
    KeyboardKeyKPEnter = 96,
    KeyboardKeyRightCtrl = 97,
    KeyboardKeyKPSlash = 98,
    KeyboardKeySysRq = 99,
    KeyboardKeyRightAlt = 100,
    KeyboardKeyLineFeed = 101,
    KeyboardKeyHome = 102,
    KeyboardKeyUp = 103,
    KeyboardKeyPageUp = 104,
    KeyboardKeyLeft = 105,
    KeyboardKeyRight = 106,
    KeyboardKeyEnd = 107,
    KeyboardKeyDown = 108,
    KeyboardKeyPageDown = 109,
    KeyboardKeyInsert = 110,
    KeyboardKeyDelete = 111,
    KeyboardKeyMacro = 112,
    KeyboardKeyMute = 113,
    KeyboardKeyVolumeDown = 114,
    KeyboardKeyVolumeUp = 115,
    KeyboardKeyPower = 116,
    KeyboardKeyKPEqual = 117,
    KeyboardKeyKPPlusMinus = 118,
    KeyboardKeyPause = 119,
    KeyboardKeyScale = 120,

    KeyboardKeyKPComma = 121,
    KeyboardKeyHangeul = 122,
    KeyboardKeyHanja = 123,
    KeyboardKeyYen = 124,
    KeyboardKeyLeftMeta = 125,
    KeyboardKeyRightMeta = 126,
    KeyboardKeyCompose = 127,

    KeyboardKeyStop = 128,
    KeyboardKeyAgain = 129,
    KeyboardKeyProps = 130,
    KeyboardKeyUndo = 131,
    KeyboardKeyFront = 132,
    KeyboardKeyCopy = 133,
    KeyboardKeyOpen = 134,
    KeyboardKeyPaste = 135,
    KeyboardKeyFind = 136,
    KeyboardKeyCut = 137,
    KeyboardKeyHelp = 138,
    KeyboardKeyMenu = 139,
    KeyboardKeyCalc = 140,
    KeyboardKeySetup = 141,
    KeyboardKeySleep = 142,
    KeyboardKeyWakeup = 143,
    KeyboardKeyFile = 144,
    KeyboardKeySendFile = 145,
    KeyboardKeyDeleteFile = 146,
    KeyboardKeyXfer = 147,
    KeyboardKeyProg1 = 148,
    KeyboardKeyProg2 = 149,
    KeyboardKeyWww = 150,
    KeyboardKeyMsdos = 151,
    KeyboardKeyScreenLock = 152,
    KeyboardKeyRotateDisplay = 153,
    KeyboardKeyCycleWindows = 154,
    KeyboardKeyMail = 155,
    KeyboardKeyBookmarks = 156,
    KeyboardKeyComputer = 157,
    KeyboardKeyBack = 158,
    KeyboardKeyForward = 159,
    KeyboardKeyCloseCd = 160,
    KeyboardKeyEjectCd = 161,
    KeyboardKeyEjectCloseCd = 162,
    KeyboardKeyNextSong = 163,
    KeyboardKeyPlayPause = 164,
    KeyboardKeyPreviousSong = 165,
    KeyboardKeyStopCd = 166,
    KeyboardKeyRecord = 167,
    KeyboardKeyRewind = 168,
    KeyboardKeyPhone = 169,
    KeyboardKeyIso = 170,
    KeyboardKeyConfig = 171,
    KeyboardKeyHomepage = 172,
    KeyboardKeyRefresh = 173,
    KeyboardKeyExit = 174,
    KeyboardKeyMove = 175,
    KeyboardKeyEdit = 176,
    KeyboardKeyScrollUp = 177,
    KeyboardKeyScrollDown = 178,
    KeyboardKeyKPLeftParen = 179,
    KeyboardKeyKPRightParen = 180,
    KeyboardKeyNew = 181,
    KeyboardKeyRedo = 182,

    KeyboardKeyF13 = 183,
    KeyboardKeyF14 = 184,
    KeyboardKeyF15 = 185,
    KeyboardKeyF16 = 186,
    KeyboardKeyF17 = 187,
    KeyboardKeyF18 = 188,
    KeyboardKeyF19 = 189,
    KeyboardKeyF20 = 190,
    KeyboardKeyF21 = 191,
    KeyboardKeyF22 = 192,
    KeyboardKeyF23 = 193,
    KeyboardKeyF24 = 194,

    KeyboardKeyPlayCd = 200,
    KeyboardKeyPauseCd = 201,
    KeyboardKeyProg3 = 202,
    KeyboardKeyProg4 = 203,
    KeyboardKeyDashboard = 204,
    KeyboardKeySuspend = 205,
    KeyboardKeyClose = 206,
    KeyboardKeyPlay = 207,
    KeyboardKeyFastForward = 208,
    KeyboardKeyBassBoost = 209,
    KeyboardKeyPrint = 210,
    KeyboardKeyHp = 211,
    KeyboardKeyCamera = 212,
    KeyboardKeySound = 213,
    KeyboardKeyQuestion = 214,
    KeyboardKeyEmail = 215,
    KeyboardKeyChat = 216,
    KeyboardKeySearch = 217,
    KeyboardKeyConnect = 218,
    KeyboardKeyFinance = 219,
    KeyboardKeySport = 220,
    KeyboardKeyShop = 221,
    KeyboardKeyAltErase = 222,
    KeyboardKeyCancel = 223,
    KeyboardKeyBrightnessDown = 224,
    KeyboardKeyBrightnessUp = 225,
    KeyboardKeyMedia = 226,

    KeyboardKeySwitchVideoMode = 227,
    KeyboardKeyKBDillumToggle = 228,
    KeyboardKeyKBDillumDown = 229,
    KeyboardKeyKBDillumUp = 230,

    KeyboardKeySend = 231,
    KeyboardKeyReply = 232,
    KeyboardKeyForwardMail = 233,
    KeyboardKeySave = 234,
    KeyboardKeyDocuments = 235,

    KeyboardKeyBattery = 236,

    KeyboardKeyBluetooth = 237,
    KeyboardKeyWlan = 238,
    KeyboardKeyUwb = 239,

    KeyboardKeyUnknown = 240,

    KeyboardKeyVideoNext = 241,
    KeyboardKeyVideoPrev = 242,
    KeyboardKeyBrightnessCycle = 243,
    KeyboardKeyBrightnessAuto = 244,
    KeyboardKeyDisplayOff = 245,

    KeyboardKeyWwan = 246,
    KeyboardKeyRfKill = 247,

    KeyboardKeyMicMute = 248,
    KeyboardKeyOk = 0x160,
    KeyboardKeySelect = 0x161,
    KeyboardKeyGoto = 0x162,
    KeyboardKeyClear = 0x163,
    KeyboardKeyPower2 = 0x164,
    KeyboardKeyOption = 0x165,
    KeyboardKeyInfo = 0x166,
    KeyboardKeyTime = 0x167,
    KeyboardKeyVendor = 0x168,
    KeyboardKeyArchive = 0x169,
    KeyboardKeyProgram = 0x16A,
    KeyboardKeyChannel = 0x16B,
    KeyboardKeyFavorites = 0x16C,
    KeyboardKeyEpg = 0x16D,
    KeyboardKeyPvr = 0x16E,
    KeyboardKeyMhp = 0x16F,
    KeyboardKeyLanguage = 0x170,
    KeyboardKeyTitle = 0x171,
    KeyboardKeySubtitle = 0x172,
    KeyboardKeyAngle = 0x173,
    KeyboardKeyFullScreen = 0x174,
    KeyboardKeyMode = 0x175,
    KeyboardKeyKeyboard = 0x176,
    KeyboardKeyAspectRatio = 0x177,
    KeyboardKeyPc = 0x178,
    KeyboardKeyTv = 0x179,
    KeyboardKeyTv2 = 0x17A,
    KeyboardKeyVcr = 0x17B,
    KeyboardKeyVcr2 = 0x17C,
    KeyboardKeySat = 0x17D,
    KeyboardKeySat2 = 0x17E,
    KeyboardKeyCd = 0x17F,
    KeyboardKeyTape = 0x180,
    KeyboardKeyRadio = 0x181,
    KeyboardKeyTuner = 0x182,
    KeyboardKeyPlayer = 0x183,
    KeyboardKeyText = 0x184,
    KeyboardKeyDvd = 0x185,
    KeyboardKeyAux = 0x186,
    KeyboardKeyMp3 = 0x187,
    KeyboardKeyAudio = 0x188,
    KeyboardKeyVideo = 0x189,
    KeyboardKeyDirectory = 0x18A,
    KeyboardKeyList = 0x18B,
    KeyboardKeyMemo = 0x18C,
    KeyboardKeyCalendar = 0x18D,
    KeyboardKeyRed = 0x18E,
    KeyboardKeyGreen = 0x18F,
    KeyboardKeyYellow = 0x190,
    KeyboardKeyBlue = 0x191,
    KeyboardKeyChannelUp = 0x192,
    KeyboardKeyChannelDown = 0x193,
    KeyboardKeyFirst = 0x194,
    KeyboardKeyLast = 0x195,
    KeyboardKeyAb = 0x196,
    KeyboardKeyNext = 0x197,
    KeyboardKeyRestart = 0x198,
    KeyboardKeySlow = 0x199,
    KeyboardKeyShuffle = 0x19A,
    KeyboardKeyBreak = 0x19B,
    KeyboardKeyPrevious = 0x19C,
    KeyboardKeyDigits = 0x19D,
    KeyboardKeyTeen = 0x19E,
    KeyboardKeyTwen = 0x19F,
    KeyboardKeyVideoPhone = 0x1A0,
    KeyboardKeyGames = 0x1A1,
    KeyboardKeyZoomIn = 0x1A2,
    KeyboardKeyZoomOut = 0x1A3,
    KeyboardKeyZoomReset = 0x1A4,
    KeyboardKeyWordProcessor = 0x1A5,
    KeyboardKeyEditor = 0x1A6,
    KeyboardKeySpreadsheet = 0x1A7,
    KeyboardKeyGraphicsEditor = 0x1A8,
    KeyboardKeyPresentation = 0x1A9,
    KeyboardKeyDatabase = 0x1AA,
    KeyboardKeyNews = 0x1AB,
    KeyboardKeyVoicemail = 0x1AC,
    KeyboardKeyAddressbook = 0x1AD,
    KeyboardKeyMessenger = 0x1AE,
    KeyboardKeyDisplayToggle = 0x1AF,
    KeyboardKeySpellcheck = 0x1B0,
    KeyboardKeyLogoff = 0x1B1,

    KeyboardKeyDollar = 0x1B2,
    KeyboardKeyEuro = 0x1B3,

    KeyboardKeyFrameBack = 0x1B4,
    KeyboardKeyFrameForward = 0x1B5,
    KeyboardKeyContextMenu = 0x1B6,
    KeyboardKeyMediaRepeat = 0x1B7,
    KeyboardKey10ChannelsUp = 0x1B8,
    KeyboardKey10ChannelsDown = 0x1B9,
    KeyboardKeyImages = 0x1BA,
    KeyboardKeyNotificationCenter = 0x1BC,
    KeyboardKeyPickupPhone = 0x1BD,
    KeyboardKeyHangupPhone = 0x1BE,

    KeyboardKeyDelEol = 0x1C0,
    KeyboardKeyDelEos = 0x1c1,
    KeyboardKeyInsLine = 0x1c2,
    KeyboardKeyDelLine = 0x1c3,

    KeyboardKeyFn = 0x1d0,
    KeyboardKeyFnEsc = 0x1d1,
    KeyboardKeyFnF1 = 0x1d2,
    KeyboardKeyFnF2 = 0x1d3,
    KeyboardKeyFnF3 = 0x1d4,
    KeyboardKeyFnF4 = 0x1d5,
    KeyboardKeyFnF5 = 0x1d6,
    KeyboardKeyFnF6 = 0x1d7,
    KeyboardKeyFnF7 = 0x1d8,
    KeyboardKeyFnF8 = 0x1d9,
    KeyboardKeyFnF9 = 0x1da,
    KeyboardKeyFnF10 = 0x1db,
    KeyboardKeyFnF11 = 0x1dc,
    KeyboardKeyFnF12 = 0x1dd,
    KeyboardKeyFn1 = 0x1de,
    KeyboardKeyFn2 = 0x1df,
    KeyboardKeyFnD = 0x1e0,
    KeyboardKeyFnE = 0x1e1,
    KeyboardKeyFnF = 0x1e2,
    KeyboardKeyFnS = 0x1e3,
    KeyboardKeyFnB = 0x1e4,
    KeyboardKeyFnRightShift = 0x1e5,

    KeyboardKeyBrlDot1 = 0x1f1,
    KeyboardKeyBrlDot2 = 0x1f2,
    KeyboardKeyBrlDot3 = 0x1f3,
    KeyboardKeyBrlDot4 = 0x1f4,
    KeyboardKeyBrlDot5 = 0x1f5,
    KeyboardKeyBrlDot6 = 0x1f6,
    KeyboardKeyBrlDot7 = 0x1f7,
    KeyboardKeyBrlDot8 = 0x1f8,
    KeyboardKeyBrlDot9 = 0x1f9,
    KeyboardKeyBrlDot10 = 0x1fa,

    KeyboardKeyNumeric0 = 0x200,
    KeyboardKeyNumeric1 = 0x201,
    KeyboardKeyNumeric2 = 0x202,
    KeyboardKeyNumeric3 = 0x203,
    KeyboardKeyNumeric4 = 0x204,
    KeyboardKeyNumeric5 = 0x205,
    KeyboardKeyNumeric6 = 0x206,
    KeyboardKeyNumeric7 = 0x207,
    KeyboardKeyNumeric8 = 0x208,
    KeyboardKeyNumeric9 = 0x209,
    KeyboardKeyNumericStar = 0x20a,
    KeyboardKeyNumericPound = 0x20b,
    KeyboardKeyNumericA = 0x20c,
    KeyboardKeyNumericB = 0x20d,
    KeyboardKeyNumericC = 0x20e,
    KeyboardKeyNumericD = 0x20f,

    KeyboardKeyCameraFocus = 0x210,
    KeyboardKeyWpsButton = 0x211,

    KeyboardKeyTouchpadToggle = 0x212,
    KeyboardKeyTouchpadOn = 0x213,
    KeyboardKeyTouchpadOff = 0x214,

    KeyboardKeyCameraZoomIn = 0x215,
    KeyboardKeyCameraZoomOut = 0x216,
    KeyboardKeyCameraUp = 0x217,
    KeyboardKeyCameraDown = 0x218,
    KeyboardKeyCameraLeft = 0x219,
    KeyboardKeyCameraRight = 0x21a,

    KeyboardKeyAttendantOn = 0x21b,
    KeyboardKeyAttendantOff = 0x21c,
    KeyboardKeyAttendantToggle = 0x21d,
    KeyboardKeyLightsToggle = 0x21e,

    KeyboardKeyAlsToggle = 0x230,
    KeyboardKeyRotateLockToggle = 0x231,
    KeyboardKeyRefreshRateToggle = 0x232,

    KeyboardKeyButtonConfig = 0x240,
    KeyboardKeyTaskManager = 0x241,
    KeyboardKeyJournal = 0x242,
    KeyboardKeyControlPanel = 0x243,
    KeyboardKeyAppSelect = 0x244,
    KeyboardKeyScreensaver = 0x245,
    KeyboardKeyVoiceCommand = 0x246,
    KeyboardKeyAssistant = 0x247,
    KeyboardKeyKbdLayoutNext = 0x248,
    KeyboardKeyEmojiPicker = 0x249,
    KeyboardKeyDictate = 0x24a,
    KeyboardKeyCameraAccessEnable = 0x24b,
    KeyboardKeyCameraAccessDisable = 0x24c,
    KeyboardKeyCameraAccessToggle = 0x24d,
    KeyboardKeyAccessibility = 0x24e,
    KeyboardKeyDoNotDisturb = 0x24f,

    KeyboardKeyBrightnessMin = 0x250,
    KeyboardKeyBrightnessMax = 0x251,

    KeyboardKeyKbdInputAssistPrev = 0x260,
    KeyboardKeyKbdInputAssistNext = 0x261,
    KeyboardKeyKbdInputAssistPrevGroup = 0x262,
    KeyboardKeyKbdInputAssistNextGroup = 0x263,
    KeyboardKeyKbdInputAssistAccept = 0x264,
    KeyboardKeyKbdInputAssistCancel = 0x265,

    /* Diagonal movement keys */
    KeyboardKeyRightUp = 0x266,
    KeyboardKeyRightDown = 0x267,
    KeyboardKeyLeftUp = 0x268,
    KeyboardKeyLeftDown = 0x269,

    KeyboardKeyRootMenu = 0x26a,
    KeyboardKeyMediaTopMenu = 0x26b,
    KeyboardKeyNumeric11 = 0x26c,
    KeyboardKeyNumeric12 = 0x26d,
    /*
     * Toggle Audio Description: refers to an audio service that helps blind and
     * visually impaired consumers understand the action in a program. Note: in
     * some countries this is referred to as "Video Description".
     */
    KeyboardKeyAudioDesc = 0x26e,
    KeyboardKey3dMode = 0x26f,
    KeyboardKeyNextFavorite = 0x270,
    KeyboardKeyStopRecord = 0x271,
    KeyboardKeyPauseRecord = 0x272,
    KeyboardKeyVod = 0x273,
    KeyboardKeyUnmute = 0x274,
    KeyboardKeyFastReverse = 0x275,
    KeyboardKeySlowReverse = 0x276,
    /*
     * Control a data application associated with the currently viewed channel,
     * e.g. teletext or data broadcast application (MHEG, MHP, HbbTV, etc.)
     */
    KeyboardKeyData = 0x277,
    KeyboardKeyOnscreenKeyboard = 0x278,
    /* Electronic privacy screen control */
    KeyboardKeyPrivacyScreenToggle = 0x279,

    /* Select an area of screen to be copied */
    KeyboardKeySelectiveScreenshot = 0x27a,

    /* Move the focus to the next or previous user controllable element within a UI container */
    KeyboardKeyNextElement = 0x27b,
    KeyboardKeyPreviousElement = 0x27c,

    /* Toggle Autopilot engagement */
    KeyboardKeyAutopilotEngageToggle = 0x27D,

    /* Shortcut Keys */
    KeyboardKeyMarkWaypoint = 0x27E,
    KeyboardKeySos = 0x27F,
    KeyboardKeyNavChart = 0x280,
    KeyboardKeyFishingChart = 0x281,
    KeyboardKeySingleRangeRadar = 0x282,
    KeyboardKeyDualRangeRadar = 0x283,
    KeyboardKeyRadarOverlay = 0x284,
    KeyboardKeyTraditionalSonar = 0x285,
    KeyboardKeyClearvuSonar = 0x286,
    KeyboardKeySidevuSonar = 0x287,
    KeyboardKeyNavInfo = 0x288,
    KeyboardKeyBrightnessMenu = 0x289,

    /*
     * Some keyboards have keys which do not have a defined meaning, these keys
     * are intended to be programmed / bound to macros by the user. For most
     * keyboards with these macro-keys the key-sequence to inject, or action to
     * take, is all handled by software on the host side. So from the kernel's
     * point of view these are just normal keys.
     *
     * The KEY_MACRO# codes below are intended for such keys, which may be labeled
     * e.g. G1-G18, or S1 - S30. The KEY_MACRO# codes MUST NOT be used for keys
     * where the marking on the key does indicate a defined meaning / purpose.
     *
     * The KEY_MACRO# codes MUST also NOT be used as fallback for when no existing
     * KEY_FOO define matches the marking / purpose. In this case a new KEY_FOO
     * define MUST be added.
     */
    KeyboardKeyMacro1 = 0x290,
    KeyboardKeyMacro2 = 0x291,
    KeyboardKeyMacro3 = 0x292,
    KeyboardKeyMacro4 = 0x293,
    KeyboardKeyMacro5 = 0x294,
    KeyboardKeyMacro6 = 0x295,
    KeyboardKeyMacro7 = 0x296,
    KeyboardKeyMacro8 = 0x297,
    KeyboardKeyMacro9 = 0x298,
    KeyboardKeyMacro10 = 0x299,
    KeyboardKeyMacro11 = 0x29a,
    KeyboardKeyMacro12 = 0x29b,
    KeyboardKeyMacro13 = 0x29c,
    KeyboardKeyMacro14 = 0x29d,
    KeyboardKeyMacro15 = 0x29e,
    KeyboardKeyMacro16 = 0x29f,
    KeyboardKeyMacro17 = 0x2a0,
    KeyboardKeyMacro18 = 0x2a1,
    KeyboardKeyMacro19 = 0x2a2,
    KeyboardKeyMacro20 = 0x2a3,
    KeyboardKeyMacro21 = 0x2a4,
    KeyboardKeyMacro22 = 0x2a5,
    KeyboardKeyMacro23 = 0x2a6,
    KeyboardKeyMacro24 = 0x2a7,
    KeyboardKeyMacro25 = 0x2a8,
    KeyboardKeyMacro26 = 0x2a9,
    KeyboardKeyMacro27 = 0x2aa,
    KeyboardKeyMacro28 = 0x2ab,
    KeyboardKeyMacro29 = 0x2ac,
    KeyboardKeyMacro30 = 0x2ad,

    /*
     * Some keyboards with the macro-keys described above have some extra keys
     * for controlling the host-side software responsible for the macro handling:
     * -A macro recording start/stop key. Note that not all keyboards which emit
     *  KEY_MACRO_RECORD_START will also emit KEY_MACRO_RECORD_STOP if
     *  KEY_MACRO_RECORD_STOP is not advertised, then KEY_MACRO_RECORD_START
     *  should be interpreted as a recording start/stop toggle;
     * -Keys for switching between different macro (pre)sets, either a key for
     *  cycling through the configured presets or keys to directly select a preset.
     */
    KeyboardKeyMacroRecordStart = 0x2b0,
    KeyboardKeyMacroRecordStop = 0x2b1,
    KeyboardKeyMacroPresetCycle = 0x2b2,
    KeyboardKeyMacroPreset1 = 0x2b3,
    KeyboardKeyMacroPreset2 = 0x2b4,
    KeyboardKeyMacroPreset3 = 0x2b5,

    /*
     * Some keyboards have a buildin LCD panel where the contents are controlled
     * by the host. Often these have a number of keys directly below the LCD
     * intended for controlling a menu shown on the LCD. These keys often don't
     * have any labeling so we just name them KEY_KBD_LCD_MENU#
     */
    KeyboardKeyKbdLcdMenu1 = 0x2b8,
    KeyboardKeyKbdLcdMenu2 = 0x2b9,
    KeyboardKeyKbdLcdMenu3 = 0x2ba,
    KeyboardKeyKbdLcdMenu4 = 0x2bb,
    KeyboardKeyKbdLcdMenu5 = 0x2bc,

    MouseButtonLeft = 0x110,
    MouseButtonRight = 0x111,
    MouseButtonMiddle = 0x112,
    MouseButtonSide = 0x113,
    MouseButtonExtra = 0x114,

    /// Base button, usually on the bottom right, Steam Quick Access Button (...)
    GamepadButtonQuick = 0x126,
    GamepadButtonQuick2 = 0x127,

    /// South action, Sony Cross x, Xbox A, Nintendo B
    GamepadButtonSouth = 0x130,
    /// East action, Sony Circle ◯, Xbox B, Nintendo A
    GamepadButtonEast = 0x131,
    /// North action, Sony Square □, Xbox X, Nintendo Y
    GamepadButtonNorth = 0x133,
    /// West action, Sony Triangle ∆, XBox Y, Nintendo X
    GamepadButtonWest = 0x134,
    /// Select, Sony Select, Xbox Back, Nintendo -, Steam Deck ⧉
    GamepadButtonSelect = 0x13a,
    /// Start, Xbox Menu, Nintendo +, Steam Deck Hamburger Menu (☰)
    GamepadButtonStart = 0x13b,
    /// Guide button, Sony PS, Xbox Home, Steam Button
    GamepadButtonGuide = 0x13c,
    /// Directional pad up
    GamepadButtonDpadUp = 0x220,
    /// Directional pad down
    GamepadButtonDpadDown = 0x221,
    /// Directional pad left
    GamepadButtonDpadLeft = 0x222,
    /// Directional pad right
    GamepadButtonDpadRight = 0x223,

    /// Left shoulder button, Xbox LB, Sony L1
    GamepadButtonLeftBumper = 0x136,
    /// Right shoulder button, Xbox RB, Sony R1
    GamepadButtonRightBumper = 0x137,
    GamepadButtonLeftTrigger = 0x138,
    GamepadButtonRightTrigger = 0x139,

    /// Z-axis button on the left stick, Sony L3, Xbox LS
    GamepadButtonLeftStick = 0x13d,
    /// Z-axis button on the right stick, Sony R3, Xbox RS
    GamepadButtonRightStick = 0x13e,

    /* Non-standard gamepad codes */
    /// Dedicated button to open an on-screen keyboard
    GamepadButtonKeyboard = 0x304,
    /// Dedicated button to take screenshots
    GamepadButtonScreenshot = 0x305,
    /// Dedicated mute button, Sony DualSense Mute
    GamepadButtonMute = 0x306,
    /// Left back paddle button, Xbox P3, Steam Deck L4
    GamepadButtonLeftPaddle1 = 0x307,
    /// Left back paddle button, Xbox P4, Steam Deck L5
    GamepadButtonLeftPaddle2 = 0x308,
    GamepadButtonLeftPaddle3 = 0x309,
    /// Right back paddle button, Xbox P1, Steam Deck R4
    GamepadButtonRightPaddle1 = 0x30a,
    /// Right back paddle button, Xbox P2, Steam Deck R5
    GamepadButtonRightPaddle2 = 0x30b,
    /// Right "side" paddle button, Legion Go M2
    GamepadButtonRightPaddle3 = 0x30c,

    /// Touch binary sensor for left stick
    GamepadButtonLeftStickTouch = 0x30d,
    /// Touch binary sensor for right stick
    GamepadButtonRightStickTouch = 0x30e,

    /// Top left button next to the left trigger, Ayaneo LC
    GamepadButtonLeftTop = 0x30f,
    /// Top left button next to the left trigger, Ayaneo RC
    GamepadButtonRightTop = 0x310,

    /// Left analog stick
    GamepadAxisLeftStick = 0x400,
    /// Right analog stick
    GamepadAxisRightStick = 0x401,

    /// Left trigger, Xbox Left Trigger, Sony L2, Nintendo ZL
    GamepadTriggerLeft = 0x500,
    /// Right trigger, Xbox Right Trigger, Sony R2, Nintendo ZR
    GamepadTriggerRight = 0x501,
    /// Left touchpad force sensor, Steam Deck left touchpad force
    GamepadTriggerLeftTouchpadForce = 0x502,
    /// Left analog stick force sensor, Steam Deck left stick force
    GamepadTriggerLeftStickForce = 0x503,
    /// Right touchpad force sensor, Steam Deck right touchpad force
    GamepadTriggerRightTouchpadForce = 0x504,
    /// Right analog stick force sensor, Steam Deck right stick force
    GamepadTriggerRightStickForce = 0x505,

    /// Left dial/wheel, Zotac Zone left dial
    GamepadDialLeft = 0x550,
    /// Right dial/wheel, Zotac Zone right dial
    GamepadDialRight = 0x551,

    /// Center or main gyro
    GyroscopeCenter = 0x600,
    /// Left side gyro
    GyroscopeLeft = 0x601,
    /// Right side gyro
    GyroscopeRight = 0x602,

    /// Center or main accelerometer
    AccelerometerCenter = 0x610,
    /// Left side accelerometer
    AccelerometerLeft = 0x611,
    /// Right side accelerometer
    AccelerometerRight = 0x612,

    /// Left touchpad touch motion
    TouchpadLeftMotion = 0x700,
    /// Center touchpad touch motion, DualSense Touchpad motion
    TouchpadCenterMotion = 0x701,
    /// Right touchpad touch motion
    TouchpadRightMotion = 0x702,
    /// Left touchpad button press
    TouchpadLeftButton = 0x703,
    /// Center touchpad button press, DualSense Touchpad button press
    TouchpadCenterButton = 0x704,
    /// Right touchpad button press
    TouchpadRightButton = 0x705,

    /// 'Main' Touchscreen touch motion
    TouchscreenMotion = 0x710,
    /// Top touchscreen motion for dualscreen touch devices
    TouchscreenTopMotion = 0x711,
}
