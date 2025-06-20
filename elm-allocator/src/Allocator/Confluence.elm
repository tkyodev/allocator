module Allocator.Confluence exposing (..)

import Allocator.Codecs
import Allocator.DataManipulation
import Allocator.Types exposing (..)
import Http
import Json.Encode
import Url


confluenceString : String
confluenceString =
    String.reverse "ecneulfnoc"


createApiUrl : { a | pageId : String } -> Url.Url -> Url.Url
createApiUrl { pageId } url =
    if String.contains confluenceString url.host then
        { url
            | path = "/" ++ confluenceString ++ "/rest/api/content/" ++ pageId
            , query = Just "status=current"
        }

    else
        url


contentBuilder : { savedAsYaml : String, dataMacroId : String, meta : Meta } -> String
contentBuilder { savedAsYaml, dataMacroId, meta } =
    String.join "\n"
        [ "<!-- --------------------------------- -->"
        , "<!--           DO NOT EDIT             -->"
        , "<!-- --------------------------------- -->"
        , "<!-- This HTML data is AUTO GENERATED. -->"
        , "<!--  Edit this page by hand only if   -->"
        , "<!--    you know what you are doing.   -->"
        , "<!-- --------------------------------- -->"
        , "<!-- {{SIGNATURE1}} -->"
        , "<!-- {{SIGNATURE2}} -->"
        , "<!-- --------------------------------- -->"
        , "<allocator-app></allocator-app>"
        , "<script>"
        , "    dataMacroId = `{{DATA_MACRO_ID}}`;"
        , "    saved = `{{SAVED_AS_YAML}}`;"
        , "</script>"
        , "<script src='https://tkyodev.github.io/allocator/allocator.{{VERSION}}.min.js'></script>"
        , "<!-- --------------------------------- -->"
        , "<!--      END OF GENERATED HTML        -->"
        , "<!-- --------------------------------- -->"
        , "<!--  Anything outside this boundary   -->"
        , "<!--          will be removed.         -->"
        , "<!-- --------------------------------- -->"
        ]
        |> String.replace "{{SAVED_AS_YAML}}" savedAsYaml
        |> String.replace "{{DATA_MACRO_ID}}" dataMacroId
        |> String.replace "{{SIGNATURE1}}" (Allocator.DataManipulation.signature1 meta)
        |> String.replace "{{SIGNATURE2}}" (Allocator.DataManipulation.signature2 meta)
        |> String.replace "{{VERSION}}" meta.version


valueBuilder : { content : String, dataMacroId : String } -> String
valueBuilder { content, dataMacroId } =
    String.join " "
        [ "<table"
        , "class=\"wysiwyg-macro\""

        -- , "style=\"background-image: ..."
        , "data-macro-name=\"html\""
        , "data-macro-id=\"{{DATA_MACRO_ID}}\""
        , "data-macro-schema-version=\"1\""
        , "data-macro-body-type=\"PLAIN_TEXT\""
        , "data-mce-resize=\"false\"><tbody><tr><td"
        , "class=\"wysiwyg-macro-body\"><pre>{{CONTENT}}</pre></td></tr></tbody></table>"
        ]
        |> String.replace "{{CONTENT}}" (escapeHtml content)
        |> String.replace "{{DATA_MACRO_ID}}" dataMacroId


escapeHtml : String -> String
escapeHtml str =
    str
        |> String.replace "&" "&amp;"
        |> String.replace "'" "&#39;"
        |> String.replace "\"" "&quot;"
        |> String.replace ">" "&gt;"
        |> String.replace "<" "&lt;"


request :
    { confluenceData : ConfluenceData
    , saved : Saved a
    , msgResponse : { savedAsYaml : String } -> Result Http.Error String -> msg
    , url : Url.Url
    , savedCounter : Int
    , meta : Meta
    }
    -> Cmd msg
request args =
    let
        savedAsYaml : String
        savedAsYaml =
            Allocator.Codecs.encodeSavedToString
                args.saved
                Allocator.Codecs.defaultSavedDataformat
    in
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "accept" "application/json, text/javascript, */*; q=0.01"
            , Http.header "x-requested-with" "XMLHttpRequest"
            ]
        , url = Url.toString <| createApiUrl args.confluenceData args.url
        , body =
            Http.stringBody "application/json; charset=UTF-8"
                (Json.Encode.encode 0
                    (encodeConfluence
                        (buildConfluenceObject
                            { content =
                                contentBuilder
                                    { savedAsYaml = savedAsYaml
                                    , dataMacroId = args.confluenceData.dataMacroId
                                    , meta = args.meta
                                    }
                            , pageId = args.confluenceData.pageId
                            , pageTitle = args.confluenceData.pageTitle
                            , pageVersion =
                                String.toInt args.confluenceData.pageVersion
                                    |> Maybe.withDefault 0
                                    |> (+) args.savedCounter
                            , parentPageId = args.confluenceData.parentPageId
                            , spaceKey = args.confluenceData.spaceKey
                            , dataMacroId = args.confluenceData.dataMacroId
                            }
                        )
                    )
                )
        , expect = Http.expectString (args.msgResponse { savedAsYaml = savedAsYaml })
        , timeout = Nothing
        , tracker = Nothing
        }


buildConfluenceObject :
    { content : String
    , pageId : String
    , pageTitle : String
    , pageVersion : Int
    , parentPageId : String
    , spaceKey : String
    , dataMacroId : String
    }
    -> Confluence
buildConfluenceObject args =
    { ancestors =
        [ { id = args.parentPageId
          , type_ = "page"
          }
        ]
    , body =
        { editor =
            { content = { id = args.pageId }
            , representation = "editor"
            , value = valueBuilder { content = args.content, dataMacroId = args.dataMacroId }
            }
        }
    , id = args.pageId
    , space =
        { key = args.spaceKey
        }
    , status = "current"
    , title = args.pageTitle
    , type_ = "page"
    , version =
        { message = ""
        , minorEdit = True
        , number = args.pageVersion + 1
        , syncRev = "dummy-sync-rev"
        }
    }



-- Created with https://korban.net/elm/json2elm/ using
-- the payload of the PUT request generated by
-- Confluence as reference.


type alias Confluence =
    { ancestors : List ConfluenceAncestorsObject
    , body : ConfluenceBody
    , id : String
    , space : ConfluenceSpace
    , status : String
    , title : String
    , type_ : String
    , version : ConfluenceVersion
    }


type alias ConfluenceAncestorsObject =
    { id : String
    , type_ : String
    }


type alias ConfluenceBody =
    { editor : ConfluenceBodyEditor
    }


type alias ConfluenceBodyEditor =
    { content : ConfluenceBodyEditorContent
    , representation : String
    , value : String
    }


type alias ConfluenceBodyEditorContent =
    { id : String
    }


type alias ConfluenceSpace =
    { key : String
    }


type alias ConfluenceVersion =
    { message : String
    , minorEdit : Bool
    , number : Int
    , syncRev : String
    }


encodeConfluence : Confluence -> Json.Encode.Value
encodeConfluence confluence =
    Json.Encode.object
        [ ( "ancestors", Json.Encode.list encodeConfluenceAncestorsObject confluence.ancestors )
        , ( "body", encodeConfluenceBody confluence.body )
        , ( "id", Json.Encode.string confluence.id )
        , ( "space", encodeConfluenceSpace confluence.space )
        , ( "status", Json.Encode.string confluence.status )
        , ( "title", Json.Encode.string confluence.title )
        , ( "type", Json.Encode.string confluence.type_ )
        , ( "version", encodeConfluenceVersion confluence.version )
        ]


encodeConfluenceAncestorsObject : ConfluenceAncestorsObject -> Json.Encode.Value
encodeConfluenceAncestorsObject confluenceAncestorsObject =
    Json.Encode.object
        [ ( "id", Json.Encode.string confluenceAncestorsObject.id )
        , ( "type", Json.Encode.string confluenceAncestorsObject.type_ )
        ]


encodeConfluenceBody : ConfluenceBody -> Json.Encode.Value
encodeConfluenceBody confluenceBody =
    Json.Encode.object
        [ ( "editor", encodeConfluenceBodyEditor confluenceBody.editor )
        ]


encodeConfluenceBodyEditor : ConfluenceBodyEditor -> Json.Encode.Value
encodeConfluenceBodyEditor confluenceBodyEditor =
    Json.Encode.object
        [ ( "content", encodeConfluenceBodyEditorContent confluenceBodyEditor.content )
        , ( "representation", Json.Encode.string confluenceBodyEditor.representation )
        , ( "value", Json.Encode.string confluenceBodyEditor.value )
        ]


encodeConfluenceBodyEditorContent : ConfluenceBodyEditorContent -> Json.Encode.Value
encodeConfluenceBodyEditorContent confluenceBodyEditorContent =
    Json.Encode.object
        [ ( "id", Json.Encode.string confluenceBodyEditorContent.id )
        ]


encodeConfluenceSpace : ConfluenceSpace -> Json.Encode.Value
encodeConfluenceSpace confluenceSpace =
    Json.Encode.object
        [ ( "key", Json.Encode.string confluenceSpace.key )
        ]


encodeConfluenceVersion : ConfluenceVersion -> Json.Encode.Value
encodeConfluenceVersion confluenceVersion =
    Json.Encode.object
        [ ( "message", Json.Encode.string confluenceVersion.message )
        , ( "minorEdit", Json.Encode.bool confluenceVersion.minorEdit )
        , ( "number", Json.Encode.int confluenceVersion.number )
        , ( "syncRev", Json.Encode.string confluenceVersion.syncRev )
        ]
