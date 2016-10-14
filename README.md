Version 1.8.0

Use the Issues page to report bugs or send them directly to lasse@bonecode.com.

<b>Note!</b> Current trunk should never be considered as a stable version. Use the latest <a href="https://github.com/bonecode/TBCEditor/releases">release</a> version instead. 

<h3>Description</h3>

A syntax highlighting edit control for RAD Studio (Delphi/C++ Builder) with code folding, completion proposal, matching pair, minimap, sync edit, multi-caret editing, word wrap, support for non-fixed-width fonts, etc. External highlighter and color scheme files are in JSON format which can be also loaded from a stream.

<h3>Build requirements</h3>

* <a href="https://github.com/ahausladen/JsonDataObjects">Json Data Objects</a> (included)
* Delphi versions XE4-8, Seattle, and Berlin are supported 
* C++ Builder versions XE7-8, Seattle, and Berlin are supported

<b>Note!</b> Do not offer me older versions to support. I don't have time to keep up with those.

<h3>Conditional compilation</h3>

Define | Description 
--- | --- 
USE_ALPHASKINS | Use <a href="http://www.alphaskins.com/">AlphaSkins</a>. AlphaSkins are most powerful theming solutions for apps developed in Delphi.
USE_VCL_STYLES | Use VCL styles. A set of graphical details that define the look and feel of a VCL application.

<h3>Usage example</h3>

```objectpascal
  with BCEditor1 do 
  begin
    Highlighter.LoadFromFile('JSON.json');
    Highlighter.Colors.LoadFromFile('Default.json'); 
    LoadFromFile(GetHighlighterFileName('JSON.json')); 
    ...
    Lines.Text := Highlighter.Info.General.Sample; 
  end;
```
<b>Note!</b> LoadFromStream does not support multi-highlighters (for example HTML with Scripts.json). Override TBCBaseEditor.CreateFileStream function, if you want to load multi-highlighters from a stream. 

<h3>Demo</h3>

TBCEditor Control Demo v. 1.8.0. 

  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo32.zip">32-bit Windows</a>
  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo64.zip">64-bit Windows</a>

The latest update: 10.10.2016 21:52, UTC+02:00

Demo source build requires 

- <a href="http://www.alphaskins.com/">AlphaSkins</a> (commercial) 
- <a href="http://wiki.delphi-jedi.org/index.php?title=JEDI_Code_Library">JCL</a> /  <a href="http://jvcl.delphi-jedi.org/">JVCL</a>
- <a href="https://github.com/bonecode/Common">Common</a>

<h3>Documentation</h3>

Documentation will be written after the project stabilizes and dust settles. This project is developed in my spare time without sources of income and as long as this is the case there is no timetable for anything. 

<h3>Projects using the control</h3>

* <a href="http://www.bonecode.com">EditBone</a>
* <a href="http://www.mitec.cz/ibq.html">MiTeC Interbase Query</a>
* <a href="http://www.mitec.cz/sqliteq.html">MiTeC SQLite Query</a>

<h3>Screenshots</h3>

![bceditor0](https://cloud.githubusercontent.com/assets/11475177/19395785/2734e6dc-9248-11e6-9d12-7bfc6c536776.png)
![bceditor1](https://cloud.githubusercontent.com/assets/11475177/19396521/2fbd3d14-924c-11e6-8e90-48694445139b.png)
![bceditor2](https://cloud.githubusercontent.com/assets/11475177/19396658/c835e2a8-924c-11e6-9af6-b347c07d1311.png)
![bceditor3](https://cloud.githubusercontent.com/assets/11475177/19397748/c80998ba-9251-11e6-854a-9427ad13b6ca.png)
![bceditor4](https://cloud.githubusercontent.com/assets/11475177/19397755/cb0f63c8-9251-11e6-8f50-784b2489ab3d.png)
![bceditor5](https://cloud.githubusercontent.com/assets/11475177/19397760/ce344e24-9251-11e6-9912-6d6e715af5e3.png)
![bceditor6](https://cloud.githubusercontent.com/assets/11475177/19397763/d1629736-9251-11e6-9f57-b7df2ecb3ecd.png)
![bceditor7](https://cloud.githubusercontent.com/assets/11475177/19397768/d5f101fc-9251-11e6-8cb6-5ee0591906d4.png)









