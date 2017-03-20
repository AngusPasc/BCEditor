unit BCEditor.Language;

interface

resourcestring
  { BCEditor.Editor }
  SBCEditorScrollInfoTopLine = 'Top line: %d';
  SBCEditorScrollInfo = '%d - %d';
  SBCEditorSearchStringNotFound = 'Search string ''%s'' not found';
  SBCEditorSearchMatchNotFound = 'Search match not found.%sRestart search from the beginning of the file?';
  SBCEditorRightMarginPosition = 'Position: %d';

  { BCEditor.MacroRecorder }
  SBCEditorCannotRecord = 'Cannot record macro; already recording or playing';
  SBCEditorCannotPlay = 'Cannot playback macro; already playing or recording';
  SBCEditorCannotPause = 'Can only pause when recording';
  SBCEditorCannotResume = 'Can only resume when paused';

  { BCEditor.Print.Preview }
  SBCEditorPreviewScrollHint = 'Page: %d';

  { BCEditor.Search }
  SBCEditorPatternIsEmpty = 'Pattern is empty';

implementation

end.
