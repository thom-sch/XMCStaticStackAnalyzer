// =============================================================================
//! @file
//! @brief      Infineon XMC4 static stack analyzer
//!
//! @author     Dipl.Inf. Thomas Schneider (c) 2020
//! @copyright  GPLv3 (https://www.gnu.org/licenses/gpl-3.0)
//!
//! @details
//!
//! StaticAnalyzer
//! --------------
//!
//! This program parses a compiler list file file and calculates the estimated stack usage of it.
//! It is build for list files compiled by ARM gcc in general and XMC4 from Infineon
//! in special with Cortex M4 architecture.
//! May be also functional for Infineon XMC1 / Cortex M0 (not tested).
//!
//! @DModuleRef{Analyzer}
//!
//! @par Credits:
//! Based on staticAnalyser by Florian Kaup, i.A. Magnetic Sense GmbH (kaup.florian@sentinelsw.de)
//! for mips gcc in general and xc32 from Microchip Technology Inc. in
//! special with MIPS32 release 5 target architecture.
//!
//! @bug
//! - inlined class methods may be encountered multiple times (once for each inlining)
//! - May also be true for overloaded functions / methods
// =============================================================================
//! @brief Analyzer unit
unit Analyzer; // --> expands to "typedef struct {} Analyzer;"
//! @DUnitGroup{Analyzer}
//! @{
//! @cond
{$IFDEF DOXYGEN}   // Not essential since default configurations don't create
                   // C++Builder *.hpp-files, so {$HPPEMIT '...'} will do nothing.
                   // Advised, if there are other configurations but "Doxygen"
                   // creating *.hpp-files.
{$HPPEMIT '//! @file Analyzer.pas'}         // causes the Delphi module file to be part of
                                            // the Doxygen's files section documentation
{$HPPEMIT '//! @file'}                      // causes the .hpp file to be part of
                                            // Doxygen's files section documentation
(*$HPPEMIT '//! @DModuleRef{Analyzer}'*)    // *.hpp file refers to associated group and struct
{$HPPEMIT '#include "doxygen.h"'}           // to replace C++Builder stuff as desired
{$ENDIF}

interface

uses Classes, Vcl.ComCtrls;

// -----------------------------------------------------------------------------
//! @endcond
//! @DType{TViewOption,Analyzer}
//! @brief selects functions to view
//! @CEnum{TViewOption,Analyzer}
//! @cond
// -----------------------------------------------------------------------------
TYPE TViewOption = (InclNoStackUsage, // include function without stack usage
                    InclSysFunc       // include system functions
                                      // (i.e. functionnames beginning with an underscore "_")
                   );
// -----------------------------------------------------------------------------
//! @endcond
//! @DType{TViewOptions,Analyzer}
//! @brief selects functions to view
//! @CType{TViewOptions,Analyzer}
//! @cond
// -----------------------------------------------------------------------------
TYPE TViewOptions = SET OF TViewOption;

PROCEDURE FillStringList(sl: TStrings; ViewOptions: TViewOptions = []);

PROCEDURE FillListView(lv: TListView; MaxDeepth: INTEGER = 5; ViewOptions: TViewOptions = []);
//PROCEDURE FillListItem(Item: TListItem);

PROCEDURE Analyse(filename: String);

PROCEDURE sortForOwn();
PROCEDURE sortForDeepest();
PROCEDURE sortForName();
PROCEDURE sortForCallDeepth();

PROCEDURE GetSU(DirName: String; Fct: TStrings); // not fully implemented

implementation

uses Windows, SysUtils;

TYPE uint32_t = Cardinal;

CONST textSectionString: String = 'Disassembly of section .text';
CONST sectionString:     String = 'Disassembly of section ';

// -----------------------------------------------------------------------------
//! @endcond
//! @DType{TFunctionFlag,Analyzer}
//! @private @memberof Analyzer
//! @brief Function flags
//! @cond
// -----------------------------------------------------------------------------
TYPE TFunctionFlag = (usesFunctionPointers,   // function uses pointers to call other functions
                      indirectFunctionCalls,  // called functions use function pointers
                      usesVariableStack,      // stack usage is variable
                      recursivlyCalled        // function is recursively called by itself
                     );

// -----------------------------------------------------------------------------
//! @endcond
//! @DType{TFunctionFlags,Analyzer}
//! @private @memberof Analyzer
//! @brief set of function flags
//! @cond
// -----------------------------------------------------------------------------
TYPE TFunctionFlags = SET OF TFunctionFlag;

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{sFlags,Analyzer} @private
//! @brief Converts @ref TFunctionFlags to string
//!
//! Replaces each flag with a single character if present, else with a blank " "
//!
//! @param FunctionFlags flags to replace
//!
//! @DMethodSource{sFlags,Analyzer}
//!
//! @cond
// -----------------------------------------------------------------------------
FUNCTION sFlags(FunctionFlags: TFunctionFlags): String;
  FUNCTION cFlag(FunctionFlag: TFunctionFlag; c: Char): Char;
  BEGIN
    IF FunctionFlag IN FunctionFlags
    THEN Result := c
    ELSE Result := ' ';
  END;
BEGIN //! sFlags
  Result := ' '
         + cFlag(usesFunctionPointers, 'P')
         + cFlag(indirectFunctionCalls,'I')
         + cFlag(usesVariableStack,    'V')
         + cFlag(recursivlyCalled,     'R')
         ;
END; //! sFlags

// -----------------------------------------------------------------------------
//! @endcond
//! @DRecordType{TFunctionInfo,Analyzer}
//! @brief Linked List Entry for function information
//!
//! This struct is used for collecting information about functions.
//! Each function has its own struct, all are linked together as a linked list.
//!
//! @DType{PFunctionInfo,Analyzer}
//! @private @memberof Analyzer
//! @brief Pointer type to @ref TFunctionInfo
//! @cond
// -----------------------------------------------------------------------------
TYPE
     PFunctionInfo = ^TFunctionInfo;

     TFunctionID = RECORD
       Name: String;
       Info: PFunctionInfo;
     END;

     TFunctionInfo = RECORD
       name:         String;          // Name of the function as it is in *.lst-file
       displayname:  String;          // Name of the function as it is in C-source
       startaddr:    uint32_t;        // Start address of the function
       endaddr:      uint32_t;        // End address of the function
       ownStack:     uint32_t;        // Estimated maximum own stack usage
       deepest:      uint32_t;        // Estimated amount of stack bytes for this function and the deepest call tree
       CalledFctIDs: ARRAY OF TFunctionID; // Name of called functions and Pointer to its FunctionInfo
       numJumpsTo:   INTEGER;         // Number of entries in ::calledFct, i.e. number of (directly) called functions
       calldeepth:   uint32_t;        // Number hierarchically called functions
       callcnt:      uint32_t;        // Number of caller's calling this function
       FunctionFlags: TFunctionFlags; //
       isProcessed:  BOOLEAN;         // Flag if deepest stack usage was calculated
       isProcessing: BOOLEAN;         // Control flag during walking through function info list
                                      // to avoid recursive endless loops:
                                      // - calculationg deepest stack usage
                                      // - building call tree
       next:         PFunctionInfo;   // Pointer to the next list member
       FUNCTION IsSysFunc(): BOOLEAN;
       FUNCTION UsesStack(): BOOLEAN;
       FUNCTION IsVisible(ViewOptions: TViewOptions): BOOLEAN;
     END; // TFunctionInfo

FUNCTION TFunctionInfo.IsSysFunc(): BOOLEAN;
BEGIN
  Result := (displayname[1] = '_')
         OR ((displayname[1] = '~') AND (displayname[2] = '_')) // Destructor!
         ;
END;
FUNCTION TFunctionInfo.UsesStack(): BOOLEAN;
BEGIN
  Result := ((OwnStack <> 0) OR (Deepest <> 0) OR (CallDeepth > 1));
END;

FUNCTION TFunctionInfo.IsVisible(ViewOptions: TViewOptions): BOOLEAN;
BEGIN
  Result :=  ((InclNoStackUsage IN ViewOptions) OR UsesStack())
         AND ((InclSysFunc IN ViewOptions) OR NOT IsSysFunc())
         ;
END;

// todo: change FunctionInfo handling from linked list to more Delphi like type
//VAR FunctionInfo: ARRAY OF TFunctionInfo;

// -----------------------------------------------------------------------------
//! @endcond
//! @DVar{firstFunctionInfo,Analyzer} @private @memberof Analyzer
//!
//! @brief Pointer to first element.
//! @cond
// -----------------------------------------------------------------------------
VAR firstFunctionInfo: PFunctionInfo;
// -----------------------------------------------------------------------------
//! @endcond
//! @DVar{FunctionInfoCnt,Analyzer} @private @memberof Analyzer
//!
//! @brief Number of @ref TFunctionInfo "FunctionInfo records"
//! @cond
// -----------------------------------------------------------------------------
VAR FunctionInfoCnt:   INTEGER;

// -----------------------------------------------------------------------------

TYPE TCallTreeItem = RECORD
       FctInfo: PFunctionInfo;
       Indent:  INTEGER;
       GrpID:   INTEGER;
     END;

VAR LinearCallTree: ARRAY OF TCallTreeItem;
    CallTreeCnt: INTEGER = 0;

PROCEDURE AddToCallTree(f: PFunctionInfo; AIndent: INTEGER; AGrpID: INTEGER);
BEGIN
  IF CallTreeCnt <= Length(LinearCallTree)
  THEN SetLength(LinearCallTree,CallTreeCnt+100);
  WITH LinearCallTree[CallTreeCnt] DO BEGIN
    FctInfo := f;
    Indent  := AIndent;
    GrpID   := AGrpID;
  END;
  Inc(CallTreeCnt);
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DProc{FreeFunctionInfo,Analyzer} @private
//!
//! @brief deletes whole function info linked list
//! @cond
// -----------------------------------------------------------------------------
PROCEDURE FreeFunctionInfo();
VAR FunctionInfo: PFunctionInfo;
BEGIN
  WHILE Assigned(firstFunctionInfo) DO BEGIN
    FunctionInfo := FirstFunctionInfo;
    FirstFunctionInfo := FirstFunctionInfo.next;
    Dispose(FunctionInfo);
  END;
  FunctionInfoCnt := 0;
  SetLength(LinearCallTree,0);
  CallTreeCnt := 0;
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{SwapFunctionInfo,Analyzer} @private
//!
//! @brief swaps two subsequent TFunctionInfo entries
//!
//! Used for sorting the list
//! @cond
// -----------------------------------------------------------------------------
FUNCTION SwapFunctionInfo(f: PFunctionInfo): PFunctionInfo;
BEGIN
  Result := f.next;
  f.next := Result.next;
  Result.next := f;
END;

PROCEDURE FillListItem(Item: TListItem);
BEGIN
  IF Item.Index < CallTreeCnt
  THEN WITH LinearCallTree[Item.Index], FctInfo^ DO BEGIN
    Item.Caption := FctInfo.displayname;
    Item.Indent  := Indent;
    Item.GroupID := GrpID;
    Item.SubItems.Add(IntToStr(OwnStack));
    Item.SubItems.Add(IntToStr(Deepest));
    Item.SubItems.Add(IntToStr(CallDeepth));
    Item.SubItems.Add(IntToStr(CallCnt));
    Item.SubItems.Add(sFlags(FunctionFlags));
  END;
END;
// -----------------------------------------------------------------------------
// Since functions were searched by name rather than by address and also
// inserted to the called function list only if they are not yet present
// the following functions are not neccessary and therefore not used.
//
// todo: uses this to parse
// - XMC's vector table to figure out interrupt functions!
// - init table to figure out static classes constructor calls
// -----------------------------------------------------------------------------
(**
 * Cleaning jumpTo table for function info.
 * The table containing jump and branch targets is cleaned and trimmed
 * by this function. All jump targets, which points into own address range
 * are discarded (hence also recursive calls), and memory is trimmed to
 * fit the final amount of jumps.
 * @param functionInfo The function info struct to clean up
 *)
(*
PROCEDURE cleanupFunctionInfo(VAR functionInfo: TFunctionInfo);
VAR
    numJumpsTo: uint32_t;
    i: INTEGER;
BEGIN
  IF NOT Assigned(@functionInfo) THEN Exit;

  numJumpsTo := 0;
  FOR i := 0 TO functionInfo.numJumpsTo-1 DO
  BEGIN
    IF ( (functionInfo.jumpsTo[i] > functionInfo.endaddr) OR
         (functionInfo.jumpsTo[i] < functionInfo.startaddr)
       )
    THEN BEGIN
      functionInfo.jumpsTo[numJumpsTo] := functionInfo.jumpsTo[i];
      Inc(numJumpsTo);
    END;
  END;
  functionInfo.numJumpsTo := numJumpsTo;
END;
*)
(**
 * Find function information by memory address.
 * This function searches the linked list for memory address.
 * The given address is checked, if it matches the address
 * range of function information.
 * @param address The address to find in function information
 * @return The function information containing the requested address
 *)
(*
FUNCTION findFunctionByAddress(address: uint32_t): PFunctionInfo;
VAR current: PFunctionInfo;
BEGIN
  current := firstFunctionInfo;
  WHILE Assigned(current) DO BEGIN
    IF ( (current.startaddr <= address) AND (current.endaddr >= address) )
    THEN break;
    current := current.next;
  END;
  Result := current;
END;
*)

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{findFunctionByName,Analyzer} @private
//! @brief Searches function info list for given name
//!
//! @param Name original(!) function name to search for
//!
//! @result Pointer to @ref TFunctionInfo or NIL if not found
//! @cond
// -----------------------------------------------------------------------------
FUNCTION findFunctionByName(Name: String): PFunctionInfo;
VAR current: PFunctionInfo;
BEGIN
  current := firstFunctionInfo;
  WHILE Assigned(current) DO BEGIN
    IF ( current.name = Name )
    THEN break;
    current := current.next;
  END;
  Result := current;
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{FriendlyName,Analyzer} @private
//! @brief deletes compiler decorations from name
//!
//! GNU-C compiler esp. decorates / converts class method names to eliminate
//! the "::" separator and/or to resolve overloaded method names. This decoration
//! is deleted here.
//!
//! Function decoration seems to be as follows:
//! - "_GLOBAL__sub_I_": optional
//! - "_Z...", where "..." <> 0..9 don't care (not figured out):
//!   if "_Z..." exist \c name consists of one or more parts, defining class names
//!   separated by "::":
//!   - NN: length of following part
//!   - NN characters: part name
//!
//!   Except of the first one for friendly name "NN" is replaced by "::". \n
//!   After the last part there is an optional signatur:
//!   - "C": constructor
//!   - "D": destructor
//!   - others: argument signatur(?) / not figured out
//!
//!   If "C" or "D" is recognized, "()" is added, for "D" the friendly name
//!   additionally is prefixed by "~".
//!
//! @cond
// -----------------------------------------------------------------------------
FUNCTION FriendlyName(CONST name: String): String;

  FUNCTION AddNamePart(i: INTEGER; VAR s: String): INTEGER;
  // Extrahiert aus s einen Namen-Teil, name[i..j] = Ziffern:
  // - extrahiert die Ziffern name[i..j] --> n
  // - hängt ggf. ein '::' und den entsprechenden Teilstring an:
  //   --> s = s + name[j+1..j+n]
  // Return: Beginn des nächsten Namen-Teils
  //         0: keine Ziffer
  VAR j,n: INTEGER;
  BEGIN
    Result := 0;
    j := i;
    WHILE (j <= Length(name)) AND (name[j] IN ['0'..'9']) DO Inc(j);
    IF j <= i THEN Exit;
    n := StrToInt(Copy(name,i,j-i));
    IF Length(s) > 0 THEN s := s+'::';
    s := s+Copy(name,j,n);
    Result := j+n;
  END;

VAR i: INTEGER;
BEGIN
  Result := name;
  i := Pos('_GLOBAL__sub_I_',name);
  IF i > 0 THEN BEGIN
    Inc(i,Length('_GLOBAL__sub_I_'));
    Delete(Result,1,i-1);
  END ELSE i := 1;
  IF (Length(name) > i+2)
  AND (name[i] = '_') AND (name[i+1] = 'Z') // schnller als i = Pos('_Z',name,i)
  THEN BEGIN
    // nächste Ziffer suchen:
    i := i+2;
    WHILE (i <= Length(name)) AND NOT (name[i] IN ['0'..'9']) DO Inc(i);
    IF i > Length(name) THEN Exit;

    Result := '';

    WHILE TRUE DO BEGIN
      i := AddNamePart(i,Result);
      IF i = 0 THEN Exit;
      IF i > Length(name) THEN Exit;

      CASE name[i] OF
        '0'..'9': ; // loop for next part
        'C': BEGIN Result :=     Result+'()'; Exit; END;
        'D': BEGIN Result := '~'+Result+'()'; Exit; END;
        ELSE Exit; // todo: argument signatur?!
      END;
    END;

  END;
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{GetFunctionName,Analyzer} @private
//! @brief extracts a function name from \c inputBuffer beginning at position \c i
//!
//! Naming conventions for Infineon XMC4 / GNU-compiler list file: \n
//! Function name sourrounded with angle brackets: <...function name...>
//!
//! @param inputBuffer assembler line = "...<...function name...>..."
//! @param i start position: i > Pos('<',...), i.e. next ">" after i-th character was found
//!
//! @result if inputBuffer meets naming conventions: function name, empty string else
//!
//! @todo Apply @ref GetFunctionName to your purposes:
//!       - extract function names from assembler line
//! @cond
// -----------------------------------------------------------------------------
FUNCTION GetFunctionName(CONST inputBuffer: String; i: INTEGER): String;
VAR j: INTEGER;
BEGIN
  Result := '';
  i := Pos('<',inputBuffer,i); IF i = 0 THEN Exit;
  j := Pos('>',inputBuffer,i); IF j = 0 THEN Exit;
  Result := Copy(inputBuffer,i+1,j-i-1);
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{CreateNewFunctionInfo,Analyzer} @private
//! @brief Create a new function info list element.
//!
//! A function info list element is allocated and filled with
//! information from disassembly label informatio0n.
//!
//! @return Pointer to a function info, filled with name and start address
//!
//! @param fctName name of function
//! @param address entry point address
//!
//! @todo Apply @ref CreateNewFunctionInfo to your purposes
//! @cond
// -----------------------------------------------------------------------------
FUNCTION CreateNewFunctionInfo(CONST fctName: String; address: uint32_t): PFunctionInfo;
BEGIN
  Result := NIL;
  IF fctName = '' THEN Exit;

  New(Result);
  Result.name        := fctName;
  Result.displayname := FriendlyName(fctName);
  Result.startaddr   := address;
  Result.endaddr     := address;
  Result.deepest     := 0;
  Result.ownStack    := 0;
//  SetLength(Result.calledFct,1000);
  SetLength(Result.CalledFctIDs,1000);
  Result.numJumpsTo  := 0;
  Result.calldeepth  := 0;
  Result.callcnt     := 0;
  Result.FunctionFlags := [];
  Result.isProcessed  := false;
  Result.isProcessing := false;
  Result.next := NIL;
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{openDisassembly,Analyzer} @private
//! @brief Create file variable for reading disassembly.
//!
//! Unlike the original MipsStaticStackAnalyzer this function directly should
//! get an assembler list file; the generation of this file should be integrated
//! to normal build process to prevent dependancies from GNU installation path!
//!
//! @param[in] filename  The assembler list file to parse
//! @param[out] f file variable
//!
//! @retval false error on opening file / file not found
//! @retval true otherwise
//!
//! @cond
// -----------------------------------------------------------------------------
FUNCTION openDisassembly(CONST filename: String; VAR f: Text): BOOLEAN;
BEGIN
  try
    Assign(f,filename);
    Reset(f);
    Result := TRUE;
  except
    Result := FALSE;
  end;
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DProc{findNextTextSection,Analyzer} @private
//! @brief Skip to next section.
//!
//! Call this function for skipping over sections, which are not ".text".
//! This is useful for skipping .rodata, .vectors and anything else with
//! no interest. The file handle is moved to the next line after the magic
//! pattern, or EOF if not found.
//!
//! @param disassemblyInput  The file handle to use
//!
//! @cond
// -----------------------------------------------------------------------------
PROCEDURE findNextTextSection(VAR disassemblyInput: Text);
VAR inputBuffer: String;
BEGIN
  WHILE NOT EOF(disassemblyInput) DO BEGIN
    ReadLn(disassemblyInput,inputBuffer);
    IF ( Pos(textSectionString,inputBuffer) > 0 ) THEN Break;
  END;
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DProc{CalcDeepestStackUsage,Analyzer} @private
//! @brief Calculate deepest stack usage for each function, calls recursively
//!
//! If the deepest stack usage is not yet calculated, it is calculated
//! synchronously. Else the precalculated stack usage is returned.
//! This function is used to update all stack information elements
//! with their deepest stack usage. This function is recursive.
//!
//! @param currentFunctionInfo The stack information for which the stack usage
//!                            has to be calculated and returned
//!
//! @return The deepest stack usage of the given stack information
//!
//! @cond
// -----------------------------------------------------------------------------
PROCEDURE CalcDeepestStackUsage(currentFunctionInfo: PFunctionInfo);

  PROCEDURE SetDeepestStackUsage(VAR target: TFunctionInfo);
  VAR i: INTEGER;
      jumpTarget: PFunctionInfo;
  BEGIN
    IF target.isProcessing
    THEN Include(target.FunctionFlags,recursivlyCalled)
    ELSE IF NOT target.isProcessed THEN BEGIN
      // avoid recursive call to this function information
      target.isProcessing := true;
      FOR i := 0 TO target.numJumpsTo-1 DO BEGIN
  //      jumpTarget := findFunctionByAddress(target.jumpsTo[i]);
        jumpTarget := target.CalledFctIDs[i].Info;
        IF NOT Assigned(jumpTarget)
        THEN jumpTarget := findFunctionByName(target.CalledFctIDs[i].Name);
        IF NOT Assigned(jumpTarget) THEN BEGIN
          // printf("Error: Jump Target not found! Function %s jumps to 0x%x\n", target->name, target->jumpsTo[i]);
          continue;
        END;
        Inc(jumpTarget.callcnt);
        SetDeepestStackUsage(jumpTarget^);
        IF (jumpTarget.deepest > target.deepest)       THEN target.deepest    := jumpTarget.deepest;
        IF (jumpTarget.calldeepth > target.calldeepth) THEN target.calldeepth := jumpTarget.calldeepth;
        IF usesFunctionPointers IN jumpTarget.FunctionFlags THEN Include(target.FunctionFlags,indirectFunctionCalls);
      END;
      Inc(target.deepest,target.ownStack);
      Inc(target.calldeepth);
      target.isProcessing := false;
      target.isProcessed  := true;
    END;
  END;

BEGIN
  WHILE Assigned(currentFunctionInfo) DO BEGIN
    SetDeepestStackUsage(currentFunctionInfo^);
    currentFunctionInfo := currentFunctionInfo.next;
  END;
END;

// -----------------------------------------------------------------------------

TYPE TcmpFunc = FUNCTION(current: PFunctionInfo): BOOLEAN;

FUNCTION cmpDeepest(current: PFunctionInfo): BOOLEAN;
BEGIN
  Result := (current.deepest < current.next.deepest);
END;
FUNCTION cmpOwn(current: PFunctionInfo): BOOLEAN;
BEGIN
  Result := (current.ownStack < current.next.ownStack);
END;
FUNCTION cmpName(current: PFunctionInfo): BOOLEAN;
BEGIN
  Result := CompareText(current.displayname,current.next.displayname) > 0;
END;
FUNCTION cmpCallDeepth(current: PFunctionInfo): BOOLEAN;
BEGIN
  Result := (current.calldeepth < current.next.calldeepth);
END;

PROCEDURE sortFor(cmpFunc: TcmpFunc);
VAR
    current: PFunctionInfo;
    i,j:     INTEGER;
BEGIN
  FOR i := FunctionInfoCnt-1 DOWNTO 0 DO BEGIN

    IF cmpFunc(firstFunctionInfo)
    THEN firstFunctionInfo := SwapFunctionInfo(firstFunctionInfo);

    current := firstFunctionInfo;

    FOR j := 1 TO i-1 DO BEGIN
      IF cmpFunc(current.next)
      THEN current.next := SwapFunctionInfo(current.next);

      current := current.next;
    END;

  END;
END;

PROCEDURE sortForName();
BEGIN
  SortFor(cmpName);
END;
(**
 * Sorts stack information linked list for deepest stack usage.
 * Descending. List pointed by ::firstStackInfo.
 *)
PROCEDURE sortForDeepest();
BEGIN
  SortFor(cmpDeepest);
END;
PROCEDURE sortForCallDeepth();
BEGIN
  SortFor(cmpCallDeepth);
END;

(**
 * Sorts stack information linked list for own stack usage.
 * Descending. List pointed by ::firstStackInfo.
 *)
PROCEDURE sortForOwn();
BEGIN
  SortFor(cmpOwn);
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DProc{FillStringList,Analyzer}
//! @brief fills a TStrings component with analyzer results table, markdown formatted
//! @CProc{FillStringList,Analyzer}
//!
//! @param[inout] sl TStrings component
//! @param ViewOptions flags to include special functions, s. @ref TViewOptions
//!                    Default: empty flags, i.e. such function are not included
//!
// -----------------------------------------------------------------------------
PROCEDURE FillStringList(sl: TStrings; ViewOptions: TViewOptions);
VAR current: PFunctionInfo;
BEGIN
  sl.Clear();
  sl.Add(Format('|%-50s|%-10s|%-10s|%-10s|%-8s|%-8s|%-50s|',
         ['Friendly Name',                                   'Own',    'Deepest','CallDeepth','CallCnt','Flags','Name']));
  sl.Add('|--------------------------------------------------|----------|----------|----------|--------|--------|--------------------------------------------------|');
  sl.BeginUpdate();
  current := firstFunctionInfo;
  WHILE Assigned(current) DO WITH current^ DO BEGIN
    IF IsVisible(ViewOptions)
    THEN sl.Add(Format('|%-50s|%10u|%10u|%10u|%8u|%-8s|%-50s|',
         [displayname, ownStack, deepest, calldeepth, callcnt, sFlags(FunctionFlags), name]));
    current := current.next;
  END;
  sl.EndUpdate();
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DProc{FillListView,Analyzer}
//! @brief fills a TListView component with analyzer results hierachically
//! @CProc{FillListView,Analyzer}
//!
//! @param[inout] lv TListView component
//! @param MaxDeepth = 5(default): max. call deepth hierachy
//! @param ViewOptions flags to include special functions, s. @ref TViewOptions
//!                    Default: empty flags, i.e. such function are not included
//!
//! @note
//! Subitem headers set by IDE: Should be done here for consistency (todo)
//! @cond
// -----------------------------------------------------------------------------
PROCEDURE FillListView(lv: TListView; MaxDeepth: INTEGER; ViewOptions: TViewOptions);
VAR ind: INTEGER;
    GrpID: INTEGER;

  PROCEDURE SetCallView(VAR target: PFunctionInfo);
  VAR i: INTEGER;
      jumpTarget: PFunctionInfo;
  BEGIN
    IF target.isProcessing THEN Exit;
    target.isProcessing := TRUE;

    WITH target^ DO
    IF IsVisible(ViewOptions) THEN BEGIN
//      AddToCallTree(target,ind,GrpID);
      WITH lv.Items.Add() DO BEGIN
        Caption := displayname;
        Indent  := ind;
        GroupID := GrpID;
        SubItems.Add(IntToStr(OwnStack));
        SubItems.Add(IntToStr(Deepest));
        SubItems.Add(IntToStr(CallDeepth));
        SubItems.Add(IntToStr(CallCnt));
        SubItems.Add(sFlags(FunctionFlags));
      END;

      IF (MaxDeepth < 0) OR (ind < MaxDeepth) THEN BEGIN
        Inc(ind);
        FOR i := 0 TO numJumpsTo-1 DO BEGIN
        jumpTarget := target.CalledFctIDs[i].Info;
        IF NOT Assigned(jumpTarget)
        THEN jumpTarget := findFunctionByName(CalledFctIDs[i].Name);
          IF NOT Assigned(jumpTarget) THEN BEGIN
            // printf("Error: Jump Target not found! Function %s jumps to 0x%x\n", target->name, target->jumpsTo[i]);
            continue;
          END;
          SetCallView(jumpTarget);
        END;
        Dec(ind);
      END;
    END;

    target.isProcessing := false;
  END;

VAR current: PFunctionInfo;
BEGIN
lv.Visible := FALSE; // sonst dauerts ewig
  lv.Clear;
  lv.Groups.Clear;

  current := firstFunctionInfo;
  WHILE Assigned(current) DO WITH current^ DO BEGIN
    ind := 0;
    IF (CallCnt = 0) AND IsVisible(ViewOptions) THEN BEGIN // not called directly by other functions
      WITH lv.Groups.Add() DO BEGIN
        State := [lgsCollapsible,lgsCollapsed];
        Header := current.displayname;
      END;
    END;
    current := current.next;
  END;

  GrpID := -1;
  current := firstFunctionInfo;
  WHILE Assigned(current) DO WITH current^ DO BEGIN
    ind := 0;
    IF (CallCnt = 0) AND IsVisible(ViewOptions) THEN BEGIN // not called directly by other functions

      Inc(GrpID);
(*
      WITH lv.Groups.Add() DO BEGIN
        State := [lgsCollapsible,lgsCollapsed];
      END;
*)
      SetCallView(current);
    END;
    current := current.next;
  END;

lv.Visible := TRUE;
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{StackGrow,Analyzer} @private
//! @brief Check for stack manipulating
//!
//! Checks if inputBuffer is an assembler mnemonic manipulating the stack.
//! If so try to figure out the stack usage.
//!
//! @param inputBuffer assembler line
//!
//! @retval > 0: number of bytes the stack increases
//! @retval = 0: i.g. not a stack operation (or stack increase = 0: should not happen)
//! @retval < 0: indirect stack manipulation (i.e. stack pointer is changed by
//!               register value).
//!
//! @note
//! - Checks only for stack growing; ignores stack decreasing instructions like
//!   pop or add instruction.
//! - note also that for XMC4 stack increases downward (stack pointer is decremented)
//!
//! @todo Apply @ref StackGrow to your purposes:
//!       - parse stack manipulation assembler instructions
//! @cond
// -----------------------------------------------------------------------------
FUNCTION StackGrow(CONST inputBuffer: String): INTEGER;
CONST
// sp as argument of an assembler instruction:
      _sp_    = #09'sp, ';            // Stack-Manipulation
      _sp1_   = #09'sp!, ';           // Stack-Manipulation

// push/pop instructions:
      _push_  = #09'push'#09'{';      // push {r2, r3},  syn: stmdb sp!,...
      _vpush_ = #09'vpush'#09'{';     // vpush {d2, d3}, syn: vstmdb sp!,...
//    _pop_   = #09'pop'#09;          // pop {r2, r3};   syn: ldmia sp!,...
//    _vpop_  = #09'vpop'#09;         // vpop {d2, d3}   syn: vldmia sp!,...

// stackpointer decrement instruction ("push"):
// direkt:                                        Beispiel:
      _sub_sp_     = #09'sub'#09'sp, #';       // sub	sp, #44	; 0x2c
      _sub_sp_sp_  = #09'sub.w'#09'sp, sp, #'; // sub.w	sp, sp, #4224	; 0x1080
      _subw_sp_sp_ = #09'subw'#09'sp, sp, #';  // subw	sp, sp, #1684	; 0x694
      _stmdbw_sp1_ = #09'stmdb.w'#09'sp!, {';  // stmdb.w	sp!, {r2, r3}
      _stmdb_sp1_  = #09'stmdb'#09'sp!, {';    // stmdb	sp!, {r2, r3}
      _vstmdb_sp1_ = #09'vstmdb'#09'sp!, {';   // vstmdb	sp!, {r2, r3}
      _vstmfd_sp1_ = #09'vstmfd'#09'sp!, {';   // vstmfd	sp!, {r2, r3}
// alles andere ist indirekt: ignorieren
                                               // sub.w	sp, sp, r2
                                               // mov	sp, ip
                                               // mov	sp, r4
                                               // str.w	sp, [pc, #148]	; 800030c <__zero_table_end__>
// stackpointer increment instruction ("pop") - ignorieren:
     _add_ = #09'add';                         // add	sp, #44	; 0x2c
                                               // add.w	sp, sp, #4224	; 0x1080
                                               // addw	sp, sp, #1684	; 0x694
     _ld_  = #09'ld';                          // ldmia.w	sp, {r1, r4}
                                               // ldr.w	sp, [r7, #32]
     _vld_ = #09'vld';                         // vldmia.w	sp, {r1, r4}

  FUNCTION GetStackGrowDirect(CONST asmCmd: String): INTEGER;
  // Result = 0: asmCmd nicht gefunden (keine/unbekannte Stack-Operation)
    FUNCTION GetStackGrow(i: INTEGER): INTEGER;
    // inputBuffer = '....#NNNN'#09'...'   NNNN = Dezimalwert <> 0
    // i = Pos('#',inputBuffer)+1
    // Result <= 0: sollte nicht vorkommen (?!)
    VAR j: INTEGER;
    BEGIN
      //Result := 0;
      j := Pos(#09,inputBuffer,i);     // j = Pos(#09,...): nach dem NNNN
      IF j = 0 THEN j := Length(inputBuffer)+1;
      Result := StrToInt(Copy(inputBuffer,i,j-i));
    END;

  BEGIN
    Result := Pos(asmCmd,inputBuffer);
    IF Result > 0 THEN Result := GetStackGrow(Result+Length(asmCmd));
  END;

  FUNCTION GetStackGrowReg(CONST asmCmd: String): INTEGER;
  // Result = 0: asmCmd nicht gefunden (keine/unbekannte Stack-Operation)
  // Result < 0: sollte nicht vorkommen

    FUNCTION GetRegGrow(i: INTEGER): INTEGER;
    // inputBuffer = '....{{...}...'   {...} = Registerliste, 4 Byte Stack / Register
    // i = Pos('{',inputBuffer)+1
    VAR j: INTEGER;
    BEGIN
      Result := 4; // Stackliste: mind. 1 Reg
      j := Pos('}',inputBuffer,i);
      WHILE i < j DO BEGIN // weitere Reg: nach dem nächsten Komma
        i := Pos(',',inputBuffer,i);
        IF i = 0 THEN Break;
        // todo: prüfen ob Reg-Bereich: rn-rm mit n,m 0..15
        Inc(Result,4);
        Inc(i);
      END;
    END;

  BEGIN
    Result := Pos(asmCmd,inputBuffer);
    IF Result > 0 THEN Result := GetRegGrow(Result+Length(asmCmd));
  END;

VAR i: INTEGER;
BEGIN
  Result := 0;

  i := Pos(_sp_,inputBuffer);
  IF i > 0 THEN BEGIN         // Stack-Befehl gefunden:
    // add/ldm/ldr/vldm/vldr (= "pop"): ignorieren
    i := Pos(_add_, inputBuffer); IF i > 0      THEN Exit;
    i := Pos(_ld_,  inputBuffer); IF i > 0      THEN Exit;
    i := Pos(_vld_, inputBuffer); IF i > 0      THEN Exit;

    Result := GetStackGrowDirect(_sub_sp_);     IF Result > 0 THEN Exit;
    Result := GetStackGrowDirect(_sub_sp_sp_);  IF Result > 0 THEN Exit;
    Result := GetStackGrowDirect(_subw_sp_sp_); IF Result > 0 THEN Exit;

    Result := -1; Exit; // indirekt
  END;

  i := Pos(_sp1_,inputBuffer);
  IF i > 0 THEN BEGIN         // Stack-Befehl gefunden:
    // stm/str: ignorieren
    i := Pos(_ld_,  inputBuffer); IF i > 0      THEN Exit;
    i := Pos(_vld_, inputBuffer); IF i > 0      THEN Exit;

    Result := GetStackGrowReg(_stmdb_sp1_);     IF Result > 0 THEN Exit;
    Result := GetStackGrowReg(_stmdbw_sp1_);    IF Result > 0 THEN Exit;
    Result := GetStackGrowReg(_vstmdb_sp1_);    IF Result > 0 THEN Exit;
    Result := GetStackGrowReg(_vstmfd_sp1_);    IF Result > 0 THEN Exit;

    Result := -1; Exit; // indirekt
  END;

  Result := GetStackGrowReg(_push_);            IF Result > 0 THEN Exit;
  Result := GetStackGrowReg(_vpush_);           IF Result > 0 THEN Exit;
  Result := 0; // keine Stackoperation
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DFunc{CalledFunction,Analyzer} @private
//! @brief Check for called function
//!
//! Checks if inputBuffer is an assembler mnemonic calling subroutines.
//!
//! @param inputBuffer assembler line
//!
//! @retval empty string: not a jump / call instruction
//! @retval single asterics '*': indirect jump / call (i.e. name of subroutine not known!)
//! @retval name of the called subroutine
//!
//! @todo Apply @ref CalledFunction to your purposes:
//!       - parse call, branch and jump assembler instructions
//! @cond
// -----------------------------------------------------------------------------
FUNCTION CalledFunction(CONST inputBuffer: String): String;
CONST _bl_    = #09'bl'#09;   // bl	8000330 <_ZN4TCCU8SetGStatEhh>
      _bw_    = #09'b.w'#09;  // b.w	8017250 <d_make_comp>

//                               jump to local address = label with offset:
//                               b.w	8017b98 <d_print_comp+0x144>
//    _bn_    = #09'b.n'#09;  // b.n	800029c <Reset_Handler+0x28>
//                               "b.n" seems always to be local

//                               indirect call:
      _blx_   = #09'blx'#09;  // blx	r3

VAR i: INTEGER;
BEGIN
  Result := '';
  i := Pos(_bl_,inputBuffer);
  IF ( i = 0 ) THEN i := Pos(_bw_,inputBuffer);
//IF ( i = 0 ) THEN i := Pos(_bn_,inputBuffer);
  IF ( i > 0 ) THEN BEGIN
    Result := GetFunctionName(inputBuffer,i);
    IF Pos('+',Result) > 0 THEN Result := ''; // local label: don't care
  END ELSE BEGIN
    i := Pos(_blx_,inputBuffer);
    IF ( i > 0 ) THEN Result := '*';          // Dummy-FunctionName
  END;
END;

//------------------------------------------------------------------------------
//! @endcond
//! @DProc{Analyse,Analyzer}
//! @brief parse *.lst disassembly and gather stack and calltree information
//! @CProc{Analyse,Analyzer}
//!
//! @param filename name of *.lst file
//!
//! For Infineon XMC4 / GNU-C *.lst files the following line structeres are
//! recognized:
//! - "AAAAAAAA:^t...assembler output..." : assembler instructions (^t = TAB = #09!)
//! - "AAAAAAAA <...function name...>:" : function entry point
//! - others: ignored
//!
//! For other µControllers / *.lst files sub-function @ref GetAddress has to be applied.
//! @sa @ref todo
//!
//! @cond
//------------------------------------------------------------------------------
PROCEDURE Analyse(filename: String  // assembler *.lst filename
                 );
CONST MinLen = 10;
VAR disassemblyInput: 	 Text;
    NewFunctionInfo:     PFunctionInfo;
    currentFunctionInfo: PFunctionInfo;
    inputBuffer:         String;
    address:             uint32_t;

  //----------------------------------------------------------------------------
  //! @endcond
  //! @DFunc{GetAddress,Analyzer} @private
  //! @brief Analyse.GetAddress: parses assembler line for address / function entry
  //!
  //! Uses upper scope Analyse.inputBuffer = assembler input line; valid lines
  //! have at least 10 characters. Caller ensures Length(inputBuffer) > 10 = MinLen!
  //!
  //! Analyses first 10 charactes:
  //! - "AAAAAAAA" = exactly 8 Hex-Chars, first chars optionally = " "
  //! - the 9th and 10th characters are either
  //!   - ":" + TAB indication an assembler line
  //!   - " " + "<" indicating a function entry point
  //!
  //! @param[out] Address recognized address
  //!
  //! @retval 0: not a valid assembler line, \c Address not valid
  //! @retval 1: assembler line
  //! @retval 2: function entry line
  //!
  //! @cond
  //----------------------------------------------------------------------------
  FUNCTION GetAddress(VAR Address: uint32_t
                     ): INTEGER;
  VAR iAddress: INTEGER absolute address;
  BEGIN
    Result := 0;
    IF (inputBuffer[1] IN[' ','0'..'9','A'..'F','a'..'f'])
    AND (inputBuffer[8] IN['0'..'9','A'..'F','a'..'f'])
    THEN BEGIN
      IF (inputBuffer[9] = ':') AND (inputBuffer[10] = #09)
      THEN Result := 2
      ELSE
      IF (inputBuffer[9] = ' ') AND (inputBuffer[10] = '<')
      THEN Result := 1;
    END;
    IF Result > 0 THEN try
      IF NOT TryStrToInt('$'+Trim(Copy(inputBuffer,1,8)),iAddress)
      THEN Result := 0;
    except
      Result := 0;
    end;
  END;

  FUNCTION AddCalledFunction(CONST CalledFct: String): BOOLEAN;
  BEGIN
    Result := CalledFct <> '';
    IF NOT Result THEN Exit;
    
    IF ( CalledFct = '*' ) // indirect function call:
    THEN Include(currentFunctionInfo.FunctionFlags,usesFunctionPointers)
    ELSE BEGIN             // direct function call
    VAR i := 0;
      // search for called function
      WHILE (i < currentFunctionInfo.numJumpsTo)
        AND (currentFunctionInfo.CalledFctIDs[i].Name <> CalledFct) DO Inc(i);
      // if not found: add to called function list
      IF i = currentFunctionInfo.numJumpsTo
      THEN BEGIN
        WITH currentFunctionInfo.CalledFctIDs[currentFunctionInfo.numJumpsTo] DO BEGIN
          Name := CalledFct;
          Info := NIL;
        END;
        Inc(currentFunctionInfo.numJumpsTo);
      END;
    END;
  END;

  FUNCTION IncStackUsage(StackUsage: INTEGER): BOOLEAN;
  BEGIN
    Result := StackUsage <> 0;  // StackUsage = 0: ..oder keine Stackoperation
    IF Result THEN BEGIN
      IF StackUsage > 0
      THEN Inc(currentFunctionInfo.ownStack,StackUsage)
      ELSE Include(currentFunctionInfo.FunctionFlags,usesVariableStack); // Ind.
    END;
  END;

BEGIN
  FreeFunctionInfo();
  IF openDisassembly(filename,disassemblyInput)
  THEN BEGIN try

    findNextTextSection(disassemblyInput);
    currentFunctionInfo := firstFunctionInfo;

    WHILE NOT EOF(disassemblyInput) DO BEGIN
    
      ReadLn(disassemblyInput,inputBuffer);
      IF (Length(inputBuffer) < MinLen) THEN Continue;

      // a new section begins
      IF Pos(sectionString,inputBuffer) <> 0 THEN BEGIN
        // If not is a text section then skip to next text section:
        IF Pos(textSectionString, inputBuffer) = 0
        THEN findNextTextSection(disassemblyInput);
        Continue;
      END;

      CASE GetAddress(address) OF
        0: ;
        1: BEGIN
             NewFunctionInfo := CreateNewFunctionInfo(GetFunctionName(inputBuffer,MinLen),address);
             IF Assigned(NewFunctionInfo) THEN BEGIN
               IF NOT Assigned(FirstFunctionInfo)
               THEN firstFunctionInfo := NewFunctionInfo
               ELSE currentFunctionInfo.next := NewFunctionInfo;
               currentFunctionInfo := NewFunctionInfo;
               Inc(FunctionInfoCnt);
             END;
           END;
        2: IF Assigned(currentFunctionInfo) THEN BEGIN
             IF (address > currentFunctionInfo.endaddr) THEN currentFunctionInfo.endaddr := address;

             IF NOT IncStackUsage(StackGrow(inputBuffer))          THEN
             IF NOT AddCalledFunction(CalledFunction(inputBuffer)) THEN
             ;
           END;
        ELSE ;
      END;
      
    END;  // WHILE NOT EOF
    except
    END;
    CloseFile(disassemblyInput);
    //---------------------------------------------------------------------------------------------
    // calculate deepest stack usage for each function, calls recursively
    CalcDeepestStackUsage(firstFunctionInfo);
  END;
END;

// -----------------------------------------------------------------------------
//! @endcond
//! @DProc{FileList,Analyzer} @private
//! @brief Helper function: walk through the given directory path and gather
//!        all files matching the given extension, optionally recursive.
//! @cond
// -----------------------------------------------------------------------------
procedure FileList(const APath, AExt: string; ARecurse: Boolean; AList: TStrings);
var
  F : TSearchRec;
  Path : string;
begin
  Path := IncludeTrailingPathDelimiter(APath); // nur für Delphi 4 und höher!
  if (ARecurse) and
    (FindFirst(Path + '*.*', faAnyFile, F) = 0) then
  try
    repeat
      if (F.Name <> '.') and (F.Name <> '..') and
        ((F.Attr and faDirectory) = faDirectory) then
        FileList(Path + F.Name, AExt, ARecurse, AList);
    until FindNext(F) <> 0;
  finally
    FindClose(F);
  end;
  if FindFirst(Path + AExt, faAnyFile, F) = 0 then
  try
    repeat
      if (F.Name <> '.') and (F.Name <> '..') and
        ((F.Attr and faDirectory) <> faDirectory) then
        AList.Add(Path + F.Name);
    until FindNext(F) <> 0;
  finally
    FindClose(F);
  end;
end;

// -----------------------------------------------------------------------------
//! @endcond
//! @DProc{GetSU,Analyzer}
//! @brief Analyses *.su files created by GNU-C compiler
//!
//! GNU-C compiler also is able to generate stack usage information
//! (use compiler-flag: -fstack-usage). Information was written for each module
//! in files with extension *.su. These may also be used.
//!
//! This function only walks through the given directory and gathers the 
//! function names into a TStrings component.
//!
//! @todo
//! Use compiler generated stack info s. @ref Analyzer.GetSU
//!
//! @cond
// -----------------------------------------------------------------------------
PROCEDURE GetSU(DirName: String; Fct: TStrings);
VAR sl: TStringList;
BEGIN
  sl := TStringList.Create();
  try
    FileList(DirName,'*.su',TRUE,sl);
    Fct.BeginUpdate;
    FOR VAR i: INTEGER := 0 TO sl.Count-1 DO BEGIN
    VAR f: Text;
    VAR s: String;
      Assign(f,sl[i]);
      Reset(f);
      WHILE NOT EOF(f) DO BEGIN
      VAR p,q,r: INTEGER;
        ReadLn(f,s);     // s = Filename:Zeile:Spalte:<[[modifier ]resutl ]Function([parameter]) stackusage type>
        p := Pos('(',s); // Beginn der Parameterliste
        IF p > 0 THEN BEGIN
          // 3x ':' überspringen:
          q := Pos(':',s);
          q := Pos(':',s,q+1);
          q := Pos(':',s,q+1);
          r := p-1;
          WHILE (r > q) AND (s[r] <> ' ') DO Dec(r);
          Fct.Add(Copy(s,r+1,p-r-1));
        END;
      END;
      Close(f);
    END;
  finally
    Fct.EndUpdate;
    sl.Free;
  END;
END;

// -----------------------------------------------------------------------------
(** todo: Command line interface
 * Main entry point.
 * Should I say more?
 * @param argc  number of arguments in argv
 * @param argv  arguments from command line
 * @return always 0
 *)
(**
 * Print stack information.
 * The first elements of stack information linked list are printed
 * to console. It is formated as a markdown compliant table, so you
 * can easily import it into doxygen or anything else documentary
 * related.
 * @param num The number of elements to print
 *)
(*
PROCEDURE printStackInfo(num: uint32_t);
BEGIN
    printf("\n|%-50s|%-15s|%-15s|%-15s|\n", "Name", "Own", "Deepest", "Indirect Calls");
    printf("|--------------------------------------------------|---------------|---------------|---------------|\n");
    functionInfo_t * current = firstFunctionInfo;
    for (uint32_t i = 0; i<num && current; i++)
    {
        printf("|%-50s|%-15i|%-15i|%-15c|\n", current->name, current->ownStack, current->deepest, current->usesFunctionPointers ? '*' : ' ');
        current = current->next;
    }
END;
*)
(*int main (int argc, char** argv)
{
    char sorting = 'd';
    uint32_t printcount = 10;
    char * filename = 0;
    for (int i=1; i<argc; i++)
    {
        if (0 == strncmp("-s",argv[i], 2))
        {
            sorting = argv[i][2];
        }
        else if (0 == strncmp("-n",argv[i], 2))
        {
            printcount = strtol(argv[i]+2, 0, 0);
        }
        else
        {
            filename = argv[i];
        }
    }

    if (filename == 0)
    {
        printf("Usage: %s [-s<sorting>] [-n<number>] <input file>\n"
                "Options:\n"
                "  -s<sorting>   Sorting of results, d=Deepest o=Own (default is %c)\n"
                "  -n<number>    The number of entries printed, -1 for all (default is %i)\n"
                "  <input file>  The ELF file to parse\n"
                "\n"
                "Report is printed as markdown table.\n"
                "Content:\n"
                "  Name:           The name of the function as the label in ELF file states.\n"
                "  Own:            The stack usage of this function by itself.\n"
                "  Deepest:        The maximum stack usage of this function and all called function.\n"
                "  Indirect Calls: This function uses function pointers, so the deepest stack usage cannot be determined.\n"
                "\n",
                argv[0],
                sorting,
                printcount);
        return -1;
    }
    Analyse(filename);
    ...etc.
 *)


initialization
finalization
  FreeFunctionInfo();
end.
//! @endcond
//! @}

