-- | This is a pretty-printer for turning the internal representation
--   of generic structured XML documents into the Doc type (which can
--   later be rendered using Text.PrettyPrint.HughesPJ.render).
--   Essentially there is one pp function for each type in
--   Text.Xml.HaXml.Types, so you can pretty-print as much or as little
--   of the document as you wish.

module Text.XML.HaXml.Pretty
  (
  -- * Pretty-print a whole document
    document
  -- ** Just one content
  ,   content
  -- ** Just one tagged element
  ,   element
  -- * Pretty-print just a DTD
  , doctypedecl
  -- ** The prolog
  ,   prolog
  -- ** A content particle description
  ,   cp
  ) where

import Prelude hiding (maybe,either)
import Data.Maybe hiding (maybe)
import Data.List (intersperse)
--import Char (isSpace)
import           Text.PrettyPrint.HughesPJ hiding ((<>))
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces

either :: (t -> t1) -> (t2 -> t1) -> Either t t2 -> t1
either f _ (Left x)  = f x
either _ g (Right x) = g x

maybe :: (t -> Doc) -> Maybe t -> Doc
maybe _ Nothing  = empty
maybe f (Just x) = f x

--peref p   = text "%" PP.<> text p PP.<> text ";"

----

document :: Document i -> Doc
prolog   :: Prolog -> Doc
xmldecl  :: XMLDecl -> Doc
misc     :: Misc -> Doc
sddecl   :: Bool -> Doc

doctypedecl :: DocTypeDecl -> Doc
markupdecl  :: MarkupDecl -> Doc
--extsubset   :: ExtSubset -> Doc
--extsubsetdecl :: ExtSubsetDecl -> Doc
cp          :: CP -> Doc

element   :: Element i -> Doc
attribute :: Attribute -> Doc                     --etc
content   :: Content i -> Doc

----

document (Document p _ e m)= prolog p $$ element e $$ vcat (map misc m)
prolog (Prolog x m1 dtd m2)= maybe xmldecl x $$
                             vcat (map misc m1) $$
                             maybe doctypedecl dtd $$
                             vcat (map misc m2)
xmldecl (XMLDecl v e sd)   = text "<?xml version='" PP.<> text v PP.<> text "'" <+>
                             maybe encodingdecl e <+>
                             maybe sddecl sd <+>
                             text "?>"
misc (Comment s)           = text "<!--" PP.<> text s PP.<> text "-->"
misc (PI (n,s))            = text "<?" PP.<> text n <+> text s PP.<> text "?>"
sddecl sd   | sd           = text "standalone='yes'"
            | otherwise    = text "standalone='no'"
doctypedecl (DTD n eid ds) = if null ds then
                                  hd PP.<> text ">"
                             else hd <+> text " [" $$
                                  vcat (map markupdecl ds) $$ text "]>"
                           where hd = text "<!DOCTYPE" <+> qname n <+>
                                      maybe externalid eid
markupdecl (Element e)     = elementdecl e
markupdecl (AttList a)     = attlistdecl a
markupdecl (Entity e)      = entitydecl e
markupdecl (Notation n)    = notationdecl n
markupdecl (MarkupMisc m)  = misc m
--markupdecl (MarkupPE p m)  = peref p

--extsubset (ExtSubset t ds) = maybe textdecl t $$
--                             vcat (map extsubsetdecl ds)
--extmarkupdecl (ExtMarkupDecl m)      = markupdecl m
--extsubsetdecl (ExtConditionalSect c) = conditionalsect c
-- -- extsubsetdecl (ExtPEReference p e)   = peref p

element (Elem n as []) = text "<" PP.<> qname n <+>
                         fsep (map attribute as) PP.<> text "/>"
element e@(Elem n as cs)
    | all isText cs    = text "<" PP.<> qname n <+> fsep (map attribute as) PP.<>
                         text ">" PP.<> hcat (map content cs) PP.<>
                         text "</" PP.<> qname n PP.<> text ">"
    | otherwise        = let (d,c) = carryelem e empty
                         in d PP.<> c

isText :: Content t -> Bool
isText (CString _ _ _) = True
isText (CRef _ _)      = True
isText _               = False

carryelem    ::  Element t  -> Doc -> (Doc, Doc)
carrycontent ::  Content t  -> Doc -> (Doc, Doc)
spancontent  :: [Content a] -> Doc -> ([Doc],Doc)

carryelem (Elem n as []) c = ( c PP.<>
                               text "<" PP.<> qname n <+> fsep (map attribute as)
                             , text "/>")
carryelem (Elem n as cs) c =  let (cs0,d0) = spancontent cs (text ">") in
                              ( c PP.<>
                                text "<" PP.<>qname n <+> fsep (map attribute as) $$
                                nest 2 (vcat cs0) PP.<>
                                d0 PP.<> text "</" PP.<> qname n
                              , text ">")

carrycontent (CElem e _) c         = carryelem e c
carrycontent (CString False s _) c = (c PP.<> chardata s, empty)
carrycontent (CString True  s _) c = (c PP.<> cdsect s, empty)
carrycontent (CRef r _) c          = (c PP.<> reference r, empty)
carrycontent (CMisc m _) c         = (c PP.<> misc m, empty)

spancontent []     c = ([],c)
spancontent (a:as) c | isText a  = let (ts,rest) = span isText (a:as)
                                       formatted = c PP.<> hcat (map content ts)
                                   in  spancontent rest formatted
                     | otherwise = let (b, c0) = carrycontent a c
                                       (bs,c1) = spancontent as c0
                                   in  (b:bs, c1)

attribute (n,v)             = qname n PP.<> text "=" PP.<> attvalue v
content (CElem e _)         = element e
content (CString False s _) = chardata s
content (CString True s _)  = cdsect s
content (CRef r _)          = reference r
content (CMisc m _)         = misc m

elementdecl :: ElementDecl -> Doc
elementdecl (ElementDecl n cs) = text "<!ELEMENT" <+> qname n <+>
                                 contentspec cs PP.<> text ">"
contentspec :: ContentSpec -> Doc
contentspec EMPTY              = text "EMPTY"
contentspec ANY                = text "ANY"
contentspec (Mixed m)          = mixed m
contentspec (ContentSpec c)    = cp c
--contentspec (ContentPE p cs)   = peref p
cp (TagName n m)       = parens (qname n) PP.<> modifier m
cp (Choice cs m)       = parens (hcat (intersperse (text "|") (map cp cs))) PP.<>
                           modifier m
cp (Seq cs m)          = parens (hcat (intersperse (text ",") (map cp cs))) PP.<>
                           modifier m
--cp (CPPE p c)          = peref p
modifier :: Modifier -> Doc
modifier None          = empty
modifier Query         = text "?"
modifier Star          = text "*"
modifier Plus          = text "+"
mixed :: Mixed -> Doc
mixed  PCDATA          = text "(#PCDATA)"
mixed (PCDATAplus ns)  = text "(#PCDATA |" <+>
                         hcat (intersperse (text "|") (map qname ns)) PP.<>
                         text ")*"

attlistdecl :: AttListDecl -> Doc
attlistdecl (AttListDecl n ds) = text "<!ATTLIST" <+> qname n <+>
                                 fsep (map attdef ds) PP.<> text ">"
attdef :: AttDef -> Doc
attdef (AttDef n t d)          = qname n <+> atttype t <+> defaultdecl d
atttype :: AttType -> Doc
atttype  StringType            = text "CDATA"
atttype (TokenizedType t)      = tokenizedtype t
atttype (EnumeratedType t)     = enumeratedtype t
tokenizedtype :: TokenizedType -> Doc
tokenizedtype ID               = text "ID"
tokenizedtype IDREF            = text "IDREF"
tokenizedtype IDREFS           = text "IDREFS"
tokenizedtype ENTITY           = text "ENTITY"
tokenizedtype ENTITIES         = text "ENTITIES"
tokenizedtype NMTOKEN          = text "NMTOKEN"
tokenizedtype NMTOKENS         = text "NMTOKENS"
enumeratedtype :: EnumeratedType -> Doc
enumeratedtype (NotationType n)= notationtype n
enumeratedtype (Enumeration e) = enumeration e
notationtype :: [String] -> Doc
notationtype ns                = text "NOTATION" <+>
                                 parens (hcat (intersperse (text "|") (map text ns)))
enumeration :: [String] -> Doc
enumeration ns                 = parens (hcat (intersperse (text "|") (map nmtoken ns)))
defaultdecl :: DefaultDecl -> Doc
defaultdecl  REQUIRED          = text "#REQUIRED"
defaultdecl  IMPLIED           = text "#IMPLIED"
defaultdecl (DefaultTo a f)    = maybe (const (text "#FIXED")) f <+> attvalue a
--conditionalsect (IncludeSect i)= text "<![INCLUDE [" <+>
--                                 vcat (map extsubsetdecl i) <+> text "]]>"
--conditionalsect (IgnoreSect i) = text "<![IGNORE [" <+>
--                                 fsep (map ignoresectcontents i) <+> text "]]>"
--ignore (Ignore)                = empty
--ignoresectcontents (IgnoreSectContents i is)
--                               = ignore i <+> vcat (map internal is)
--                          where internal (ics,i) = text "<![[" <+>
--                                                   ignoresectcontents ics <+>
--                                                   text "]]>" <+> ignore i
reference :: Reference -> Doc
reference (RefEntity er)       = entityref er
reference (RefChar cr)         = charref cr
entityref :: String -> Doc
entityref n                    = text "&" PP.<> text n PP.<> text ";"
charref :: (Show a) => a -> Doc
charref c                      = text "&#" PP.<> text (show c) PP.<> text ";"
entitydecl :: EntityDecl -> Doc
entitydecl (EntityGEDecl d)    = gedecl d
entitydecl (EntityPEDecl d)    = pedecl d
gedecl :: GEDecl -> Doc
gedecl (GEDecl n ed)           = text "<!ENTITY" <+> text n <+> entitydef ed PP.<>
                                 text ">"
pedecl :: PEDecl -> Doc
pedecl (PEDecl n pd)           = text "<!ENTITY %" <+> text n <+> pedef pd PP.<>
                                 text ">"
entitydef :: EntityDef -> Doc
entitydef (DefEntityValue ew)  = entityvalue ew
entitydef (DefExternalID i nd) = externalid i <+> maybe ndatadecl nd
pedef :: PEDef -> Doc
pedef (PEDefEntityValue ew)    = entityvalue ew
pedef (PEDefExternalID eid)    = externalid eid
externalid :: ExternalID -> Doc
externalid (SYSTEM sl)         = text "SYSTEM" <+> systemliteral sl
externalid (PUBLIC i sl)       = text "PUBLIC" <+> pubidliteral i <+>
                                 systemliteral sl
ndatadecl :: NDataDecl -> Doc
ndatadecl (NDATA n)            = text "NDATA" <+> text n
--textdecl (TextDecl vi ed)      = text "<?xml" <+> maybe text vi <+>
--                                 encodingdecl ed <+> text "?>"
--extparsedent (ExtParsedEnt t c)= maybe textdecl t <+> content c
--extpe (ExtPE t esd)            = maybe textdecl t <+>
--                                 vcat (map extsubsetdecl esd)
notationdecl :: NotationDecl -> Doc
notationdecl (NOTATION n e)    = text "<!NOTATION" <+> text n <+>
                                 either externalid publicid e PP.<>
                                 text ">"
publicid :: PublicID -> Doc
publicid (PUBLICID p)          = text "PUBLIC" <+> pubidliteral p
encodingdecl :: EncodingDecl -> Doc
encodingdecl (EncodingDecl s)  = text "encoding='" PP.<> text s PP.<> text "'"
nmtoken :: String -> Doc
nmtoken s                      = text s
attvalue :: AttValue -> Doc
attvalue (AttValue esr)        = text "\"" PP.<>
                                 hcat (map (either text reference) esr) PP.<>
                                 text "\""
entityvalue :: EntityValue -> Doc
entityvalue (EntityValue evs)
  | containsDoubleQuote evs    = text "'"  PP.<> hcat (map ev evs) PP.<> text "'"
  | otherwise                  = text "\"" PP.<> hcat (map ev evs) PP.<> text "\""
ev :: EV -> Doc
ev (EVString s)                = text s
--ev (EVPERef p e)               = peref p
ev (EVRef r)                   = reference r
pubidliteral :: PubidLiteral -> Doc
pubidliteral (PubidLiteral s)
    | '"' `elem` s             = text "'" PP.<> text s PP.<> text "'"
    | otherwise                = text "\"" PP.<> text s PP.<> text "\""
systemliteral :: SystemLiteral -> Doc
systemliteral (SystemLiteral s)
    | '"' `elem` s             = text "'" PP.<> text s PP.<> text "'"
    | otherwise                = text "\"" PP.<> text s PP.<> text "\""
chardata :: String -> Doc
chardata s                     = {-if all isSpace s then empty else-} text s
cdsect :: String -> Doc
cdsect c                       = text "<![CDATA[" PP.<> chardata c PP.<> text "]]>"

qname n                        = text (printableName n)

----
containsDoubleQuote :: [EV] -> Bool
containsDoubleQuote evs = any csq evs
    where csq (EVString s) = '"' `elem` s
          csq _            = False
