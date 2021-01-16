// =============================================================================
// Hilfsdatei zur Kommentierung von Delphi-Quelltexten mit Doxygen
//
// Diese Datei sollte per HPPEMIT in die von Delphi erzeugten HPP-Dateien
// includiert werden
// =============================================================================

#ifdef DOXYGEN	// PREDEFINED by Doxygen configuration!

// -----------------------------------------------------------------------------
// Eliminierung von C-Builder-Besonderheiten
// -----------------------------------------------------------------------------
#define DELPHI_PACKAGE
#define __published public
#define __property property
#define __fastcall
#define extern
#define DECLSPEC_DRECORD
#define _ANNOT_ATTR_NC
#define HIDESBASE
#define DECLSPEC_DENUM
#define _DELPHI_SET_CHAR

// -----------------------------------------------------------------------------
// replace C++ builder identifiers with their Delphi pendants:
// -----------------------------------------------------------------------------
#define UnicodeString	String
#define Word			WORD

#endif
// =============================================================================

