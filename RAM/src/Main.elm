port module Main exposing (..)

import Browser
import Html exposing (Html, div, textarea, button, text, table, th, tr, td,span,input)
import Html.Attributes exposing (class, placeholder, value,style, min ,step,type_)
import Html.Events exposing (onClick, onInput)
import Regex exposing (fromString, find,Regex)
import Html.Attributes as Attr
import Time exposing (Posix)
import Html exposing (thead)
import Html exposing (tbody)
import Html.Attributes exposing (id)
import Html exposing (p)
import Html exposing (h2)
import Html exposing (ul)
import Html exposing (li)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Decode

port importCode  : () -> Cmd msg
port exportCode : String -> Cmd msg
port receiveCode : (String -> msg) -> Sub msg

port scrollToElement : String -> Cmd msg  

port scrollToRegister : String -> Cmd msg

-- MODEL

type alias Model =
    { code : String
    , registers : List Register
    , tape : List String
    , commands : List String  -- Список команд, полученных из кода
    , tapeReadIndex : Int
    , outputTape : List String
    , currentStep : Int
    , isStepExecution :Bool
    , previousRegisters : List Register
    , sliderValue : Float
    , isRunning : Bool
    , errorMessage : Maybe String
    , errorStep : Maybe Int
    , amountOfExecutedCommands : Int
    , changedRegisters : List Int
    , showHelpModal : Bool

    }

type alias Register =
    { number : Int
    , value : Int
    }


initialModel : Model
initialModel =
    { code = ""
    , registers = List.range 0 100 |> List.map (\n -> { number = n, value = 0 })
    , tape = []
    , commands = []
    , tapeReadIndex = 0
    , outputTape = []
    , currentStep = -1
    , isStepExecution = False
    , previousRegisters = []
    , sliderValue = 5
    , isRunning = False 
    , errorMessage = Nothing
    , errorStep = Nothing
    , amountOfExecutedCommands = 0
    , changedRegisters = []
    , showHelpModal = False
    }


-- UPDATE

type Msg
    = UpdateCode String
    | ExportCode
    | ImportCode
    | CodeReceived String
    | AddTapeField
    | UpdateTapeField Int String
    | CompileCode
    | UpdateOutputTapeField Int String
    | Step
    | SliderChanged String
    | ExecuteAction
    | StopExecution
    | CloseError
    | DeleteTapeField Int
    | ToggleHelpModal
    | IgnoredSliderChange String
    | NoOp

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        interval =
            case round model.sliderValue of  -- ✅ Округляем `Float` в `Int`
                1 -> 1000
                2 -> 700
                3 -> 500
                4 -> 300
                _ -> 1000  -- ✅ Значение по умолчанию

    in
    Sub.batch
        [ receiveCode CodeReceived  
        , if model.isRunning &&  model.currentStep < List.length model.commands then
            
            Time.every (toFloat interval) (always Step)  -- ✅ Преобразуем `Int` в `Float`
          else
            Sub.none  -- ❌ Останавливаем таймер
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCode newCode ->
            let
                newCommands =
                    getAllCommands newCode
                _= Debug.log "Parsed Commands" newCommands
                _= Debug.log "Tape" model.tape
            in
            ( { model
                | code = newCode
                , commands = newCommands
              }
            , Cmd.none
            )

        ExportCode ->
            (model, exportCode model.code)

        ImportCode ->
            ( model, importCode() )

        CodeReceived importedCode ->
            ( { model | code = importedCode }, Cmd.none )    

        AddTapeField ->
            ({ model | tape = model.tape ++ [""] }, Cmd.none)

        UpdateTapeField index newValue ->
            ({ model | tape = List.indexedMap (\i val -> if i == index then newValue else val) model.tape }, Cmd.none)

        CompileCode ->
            let
                _= Debug.log "model.tape" model.tape 
                executionState =
                    executeCommands model.commands model.registers model.tape model.outputTape model.tapeReadIndex model.commands 0 False 0 []

                newChangedRegisters =
                            unique executionState.changedRegisters
                            
                _ = Debug.log "pocet instrukcii" executionState.amountOfExecutedCommands 
                _ = Debug.log "pamatova zlozitost" (List.length newChangedRegisters) 
            in
            ( { model
                | registers = executionState.registers
                , outputTape = executionState.outputTape  -- ❗ Теперь обновляется outputTape!
                , tapeReadIndex = executionState.tapeReadIndex
                , currentStep = executionState.currentStep
                , errorMessage = executionState.errorMessage
                , errorStep = executionState.errorStep 
                , amountOfExecutedCommands = executionState.amountOfExecutedCommands
                , changedRegisters = executionState.changedRegisters
            }
            , Cmd.none
            )

        UpdateOutputTapeField index value ->
            ( { model
                | outputTape = List.indexedMap (\i v -> if i == index then value else v) model.outputTape
              }
            , Cmd.none
            )    
        Step ->
            if model.currentStep  < List.length model.commands then
                let
                    updatedIsRunning =
                        case executionState.errorMessage of
                            Just _ -> False  -- ❌ Останавливаем выполнение при ошибке
                            Nothing -> model.isRunning
                    command = getAt model.currentStep model.commands
                    -- _ = Debug.log " model.commands"  executionState.currentStep
                    _ = Debug.log " model.currentStep"  executionState.currentStep
                    executionState =
                        
                        if model.currentStep == -1 then
                            { registers = List.range 0 100 |> List.map (\n -> { number = n, value = 0 })
                            , tape = model.tape
                            , outputTape = []
                            , tapeReadIndex = model.tapeReadIndex
                            , currentStep = 0  -- ✅ Переход к шагу 0
                            , errorMessage = Nothing
                            , errorStep = Nothing
                            , amountOfExecutedCommands = 0
                            , changedRegisters = []
                            }
                        else if model.currentStep >= 0 then
                            case command of
                                Just cmd -> executeCommands [cmd] model.registers model.tape model.outputTape model.tapeReadIndex model.commands model.currentStep model.isStepExecution model.amountOfExecutedCommands model.changedRegisters
                                Nothing -> { registers = model.registers, tape = model.tape, outputTape = model.outputTape, tapeReadIndex = model.tapeReadIndex, currentStep = model.currentStep, errorMessage = Nothing , errorStep = Nothing  , amountOfExecutedCommands = model.amountOfExecutedCommands , changedRegisters = model.changedRegisters}  
                        else
                            { registers = model.registers, tape = model.tape, outputTape = model.outputTape, tapeReadIndex = model.tapeReadIndex, currentStep = model.currentStep + 1 , errorMessage = Nothing , errorStep = Nothing, amountOfExecutedCommands = model.amountOfExecutedCommands , changedRegisters = model.changedRegisters} 
                    newStep = executionState.currentStep
                    -- Найти изменённый регистр
                    changedRegister =
                        executionState.registers
                            |> List.filter (\r -> 
                                case List.filter (\prev -> prev.number == r.number) model.registers of
                                    [prev] -> prev.value /= r.value
                                    _ -> False
                            )
                            |> List.head
                    cmdScrollCommand = scrollToElement ("row-" ++ String.fromInt newStep)
                    
                    cmdScrollRegister =
                        case changedRegister of
                            Just reg -> scrollToRegister ("reg-" ++ String.fromInt reg.number)  
                            Nothing -> Cmd.none
                    _ = Debug.log "model.errorStep AFTER execution" executionState.errorStep  -- ✅ Проверяем, изменяется ли errorStep

                    

                    tapeReadIndex2 =         
                        if executionState.currentStep  == List.length model.commands then

                            0
                        else
                            executionState.tapeReadIndex
                    updatedIsRunning2 =  
                        if executionState.currentStep  == List.length model.commands then
                            False
                        else
                            updatedIsRunning
                    newChangedRegisters =
                        if executionState.currentStep == List.length model.commands then
                            unique executionState.changedRegisters
                        else
                            executionState.changedRegisters

                    newStep2 =
                        if executionState.currentStep  == List.length model.commands then
                            -1
                        else
                            executionState.currentStep       
                    
                in
                ({ model 
                    | previousRegisters = model.registers
                    , registers = executionState.registers
                    , tape = executionState.tape
                    , outputTape = executionState.outputTape
                    , tapeReadIndex = tapeReadIndex2
                    , currentStep =newStep2
                    , isStepExecution = True
                    , errorMessage = executionState.errorMessage
                    , errorStep = executionState.errorStep
                    , isRunning = updatedIsRunning2
                    , amountOfExecutedCommands = executionState.amountOfExecutedCommands
                    , changedRegisters = executionState.changedRegisters

                }
                , Cmd.batch [ cmdScrollCommand, cmdScrollRegister ]  )    

            else
                let
                               
                    updatedModel = { model |isStepExecution = True, isRunning = False, currentStep =-1, tapeReadIndex= 0 }  
                in

                (updatedModel, Cmd.none)  -- Если команды закончились, не делаем ничего
            
        SliderChanged newValue ->
            ( { model | sliderValue = String.toFloat newValue |> Maybe.withDefault model.sliderValue }
            , Cmd.none
            )
        ExecuteAction ->
           
            if model.sliderValue < 5 then
                let
                    updatedModel = { model | isRunning = True , currentStep =-1, tapeReadIndex= 0 , registers = List.range 0 100 |> List.map (\n -> { number = n, value = 0 }),outputTape = []}  
                in
                update Step updatedModel      
            else 
                let
                    updatedModel = { model | isRunning = False, currentStep =-1, tapeReadIndex= 0 , registers = List.range 0 100 |> List.map (\n -> { number = n, value = 0 }),outputTape = []}  
                in
                update CompileCode updatedModel     
        StopExecution ->
            ( { model 
                | currentStep = -1
                , isStepExecution = False
                , isRunning = False
                , tapeReadIndex = 0
                , errorStep = Nothing
              }
            , Cmd.none
            )      
        CloseError ->
            ({ model | errorMessage = Nothing }, Cmd.none) 

        DeleteTapeField index ->
            let
                newTape =
                    List.take index model.tape ++ List.drop (index + 1) model.tape
            in
            ({ model | tape = newTape }, Cmd.none)

        ToggleHelpModal ->
            ( { model | showHelpModal = not model.showHelpModal }, Cmd.none )
            
        IgnoredSliderChange _->
            (model, Cmd.none)
        NoOp ->
            (model, Cmd.none)
unique : List comparable -> List comparable
unique list =
    List.foldl (\x acc -> if List.member x acc then acc else x :: acc) [] list


getAt : Int -> List a -> Maybe a
getAt index list =
    list |> List.drop index |> List.head

type alias ExecutionState =
    { registers : List Register
    , tape : List String
    , outputTape : List String
    , tapeReadIndex : Int
    , currentStep : Int
    , errorMessage : Maybe String
    , errorStep : Maybe Int
    , amountOfExecutedCommands : Int
    , changedRegisters : List Int
    }


executeCommands : List String -> List Register -> List String -> List String -> Int -> List String -> Int -> Bool -> Int -> List Int-> ExecutionState
executeCommands cmds registers tape outputTape readIndex allCommands currentStep isStepExecution amountOfExecutedCommands changedRegisters =
    if amountOfExecutedCommands >= 20000 then
        { registers = registers
        , tape = tape
        , outputTape = outputTape
        , tapeReadIndex = readIndex
        , currentStep = currentStep
        , errorMessage = Just "⛔ Превышен лимит шагов. Возможен бесконечный цикл."
        , errorStep = Just currentStep
        , amountOfExecutedCommands = amountOfExecutedCommands
        , changedRegisters = changedRegisters
        }
    else
    case cmds of
        []  ->
            { registers = registers
            , tape = tape
            , outputTape = outputTape
            , tapeReadIndex = readIndex
            , currentStep = currentStep 
            , errorMessage = Nothing
            , errorStep = Nothing
            , amountOfExecutedCommands = amountOfExecutedCommands
            , changedRegisters = changedRegisters
            }

        "halt" :: _ ->
            { registers = registers
            , tape = tape
            , outputTape = outputTape
            , tapeReadIndex = readIndex
            , currentStep = currentStep + 1
            , errorMessage = Nothing
            , errorStep = Nothing
            , amountOfExecutedCommands = amountOfExecutedCommands + 1
            , changedRegisters = changedRegisters
            }

        cmd :: rest ->
            let
                _= Debug.log "cmds" rest
            in 
            case String.words cmd of
                -- READ n: Читаем значение из tape и записываем в регистр n
                ["read", numStr] ->
                    if String.contains "=" numStr then
                        createErrorState "Chyba: nesprávny formát príkazu READ. Použite READ <číslo registra> alebo READ *<číslo registra>." currentStep registers tape outputTape readIndex (changedRegisters)
                    else
                    case resolveRegisterAddress numStr registers of
                        Just regNum ->
                            let
                                registerExists = List.any (\r -> r.number == regNum) registers
                            in
                            if registerExists then
                                case List.drop readIndex tape |> List.head of
                                    Just firstValue ->
                                        case String.toInt firstValue of
                                            Just intVal ->   
                                                let
                                                    updatedRegisters = updateRegister regNum (String.fromInt intVal) registers
                                                in
                                                if isStepExecution then
                                                    { registers = updatedRegisters
                                                    , tape = tape
                                                    , outputTape = outputTape 
                                                    , tapeReadIndex = readIndex + 1
                                                    , currentStep = currentStep + 1
                                                    , errorMessage = Nothing
                                                    , errorStep = Nothing
                                                    , amountOfExecutedCommands = amountOfExecutedCommands + 1
                                                    , changedRegisters = changedRegisters
                                                    
                                                    }
                                                else
                                                    executeCommands rest updatedRegisters tape outputTape (readIndex + 1) allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (regNum :: changedRegisters)
                                            Nothing ->
                                                createErrorState
                                                    ("Chyba: hodnota \"" ++ firstValue ++ "\" zo vstupnej pásky nie je platné číslo.")
                                                    currentStep registers tape outputTape readIndex changedRegisters
                                    Nothing ->
                                        createErrorState "Chyba: všetky hodnoty zo vstupnej pásky boli prečítané" currentStep registers tape outputTape readIndex (changedRegisters)
                            else
                                
                                createErrorState ("Chyba: register  " ++ numStr ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)

                        Nothing ->
                            createErrorState "Chyba: nesprávny formát príkazu READ. Použite READ <číslo registra> alebo READ *<číslo registra>." currentStep registers tape outputTape readIndex (changedRegisters)

                -- WRITE n: Записываем значение из регистра n в outputTape
                ["write", numStr] ->
                    if String.contains "=" numStr then
                        createErrorState "Chyba: nesprávny formát príkazu WRITE. Použite WRITE <číslo registra> alebo WRITE *<číslo registra>." currentStep registers tape outputTape readIndex (changedRegisters)
                    else
                    case resolveRegisterAddress numStr registers of
                        Just realRegNum ->
                            -- Получаем значение из регистра realRegNum
                            case List.head (List.filter (\r -> r.number == realRegNum) registers) of
                                Just reg ->
                                    let
                                        newOutputTape = outputTape ++ [String.fromInt reg.value]
                                    in
                                    if isStepExecution then
                                        { registers = registers
                                        , tape = tape
                                        , outputTape = newOutputTape
                                        , tapeReadIndex = readIndex + 1
                                        , currentStep = currentStep + 1 
                                        , errorMessage = Nothing  
                                        , errorStep = Nothing 
                                        , amountOfExecutedCommands = amountOfExecutedCommands + 1    
                                        , changedRegisters = changedRegisters                                
                                        }
                                    else
                                    executeCommands rest registers tape newOutputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                                Nothing ->
                                    createErrorState ("Chyba: register  " ++ numStr ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)
                        Nothing ->
                            createErrorState "Chyba: nesprávny formát príkazu WRITE. Použite WRITE <číslo registra> alebo WRITE *<číslo registra>." currentStep registers tape outputTape readIndex (changedRegisters)
                ["load", numStr] ->
                    case resolveRegisterAddress numStr registers of
                        Just regNum ->
                            let 
                                registerExists = List.any (\r -> r.number == regNum) registers
                            in
                            if registerExists then
                                let 
                                    realValue =
                                        case String.uncons numStr of
                                            Just ('=', constValue) -> constValue -- Убираем '=' и оставляем число
                                            _ -> 
                                                case List.head (List.filter (\r -> r.number == regNum) registers) of
                                                    Just reg -> String.fromInt reg.value  -- Значение из регистра
                                                    Nothing -> "0"  -- Если регистра нет, записываем 0
                                    updatedRegisters = updateRegister 0 realValue registers
                                in
                                executeCommands rest updatedRegisters tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (0 :: changedRegisters)
                            else 
                                createErrorState ("Chyba: register  " ++ numStr ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)

                        Nothing ->
                            -- Ошибка парсинга, продолжаем выполнение
                            createErrorState "Chyba: nesprávny formát príkazu LOAD. Použite LOAD <číslo registra> , LOAD *<číslo registra> alebo LOAD =<konštanta>." currentStep registers tape outputTape readIndex (changedRegisters)

                ["store", numStr] ->
                    if String.contains "=" numStr then
                        createErrorState "Chyba: nesprávny formát príkazu STORE. Použite STORE <číslo registra> alebo STORE *<číslo registra>." currentStep registers tape outputTape readIndex (changedRegisters)
                    else
                    case resolveRegisterAddress numStr registers of
                        Just regNum ->
                            let
                                maybeTargetReg = List.filter (\r -> r.number == regNum) registers |> List.head
                            in
                            case maybeTargetReg of
                                Just _ ->
                                    -- Найти значение в регистре 0
                                    case List.head (List.filter (\r -> r.number == 0) registers) of
                                        Just regZero ->
                                            -- ✅ Обновляем `regNum` значением из регистра 0
                                            let
                                                updatedRegisters = updateRegister regNum (String.fromInt regZero.value) registers
                                            in
                                            executeCommands rest updatedRegisters tape outputTape readIndex allCommands (currentStep + 1) isStepExecution (amountOfExecutedCommands + 1) (regNum :: changedRegisters)
                                        Nothing ->
                                            -- ❌ Если регистра 0 нет (маловероятно), продолжаем выполнение
                                            executeCommands rest registers tape outputTape readIndex allCommands (currentStep + 1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                                Nothing ->
                                    -- ❌ Ошибка: `regNum` не найден в `registers`
                                    createErrorState ("Chyba: register  " ++ numStr ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)
                        Nothing ->
                            -- ❌ Ошибка: `STORE` вызван с неправильным аргументом (например, `STORE X`)
                            createErrorState "Chyba: nesprávny formát príkazu STORE. Použite STORE <číslo registra> alebo STORE *<číslo registra>." currentStep registers tape outputTape readIndex (changedRegisters)
         
                ["add", numStr] ->
                    case resolveRegisterAddress numStr registers of
                        Just rawValue ->  
                            if rawValue == -1 then
                                createErrorState ("Chyba: register  " ++ numStr ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)
                            else
                                let
                                    -- Проверяем, является ли numStr константой (начинается с '=')
                                    valueToAdd =
                                        case String.uncons numStr of
                                            Just ('=', _) -> rawValue  -- ❗ Константа: оставляем как есть
                                            _ ->
                                                -- ❗ Обычная или непрямая адресация: ищем значение в `registers[rawValue]`
                                                case List.head (List.filter (\r -> r.number == rawValue) registers) of
                                                    Just reg -> reg.value  -- ✅ Значение из регистра
                                                    Nothing -> -1  -- ❌ Ошибка: регистра нет
                                in

                                case List.head (List.filter (\r -> r.number == 0) registers) of
                                    Just regZero ->
                                        let
                                            newValue = regZero.value + valueToAdd
                                            updatedRegisters = updateRegister 0 (String.fromInt newValue) registers
                                        in
                                        executeCommands rest updatedRegisters tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (0 :: changedRegisters)
                                    Nothing ->
                                        -- Если регистра 0 нет (маловероятно), продолжаем выполнение
                                        executeCommands rest registers tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                        Nothing ->
                            -- ❌ Ошибка парсинга (`ADD x` вместо числа)
                            createErrorState "Chyba: nesprávny formát príkazu ADD. Použite ADD <číslo registra> , ADD *<číslo registra> alebo ADD =<konštanta>." currentStep registers tape outputTape readIndex (changedRegisters)
                ["sub", numStr] ->
                    case resolveRegisterAddress numStr registers of
                        Just rawValue ->  
                            if rawValue == -1 then
                                createErrorState ("Chyba: register  " ++ numStr ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)
                            else
                            let
                                -- Проверяем, является ли numStr константой (начинается с '=')
                                valueToAdd =
                                    case String.uncons numStr of
                                        Just ('=', _) -> rawValue  -- ❗ Константа: оставляем как есть
                                        _ ->
                                            -- ❗ Обычная или непрямая адресация: ищем значение в `registers[rawValue]`
                                            case List.head (List.filter (\r -> r.number == rawValue) registers) of
                                                Just reg -> reg.value  -- Значение из регистра
                                                Nothing -> 0  -- Если регистра нет, используем 0

                            in
                            -- Найти значение в регистре 0
                            case List.head (List.filter (\r -> r.number == 0) registers) of
                                Just regZero ->
                                    -- ❗ Обновляем регистр 0, добавляя к нему `valueToAdd`
                                    let
                                        newValue = regZero.value - valueToAdd
                                        updatedRegisters = updateRegister 0 (String.fromInt newValue) registers
                                    in
                                    executeCommands rest updatedRegisters tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (0 :: changedRegisters)

                                Nothing ->
                                    -- Если регистра 0 нет (маловероятно), продолжаем выполнение
                                    executeCommands rest registers tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1)  (changedRegisters)   

                        Nothing ->
                            -- Ошибка парсинга (например, `ADD x`), продолжаем выполнение
                            createErrorState "Chyba: nesprávny formát príkazu SUB. Použite SUB <číslo registra> , SUB *<číslo registra> alebo SUB =<konštanta>." currentStep registers tape outputTape readIndex (changedRegisters)
                ["mul", numStr] ->
                    case resolveRegisterAddress numStr registers of
                        Just rawValue ->  
                            if rawValue == -1 then
                                createErrorState ("Chyba: register  " ++ numStr ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)
                            else

                            let
                                -- Проверяем, является ли numStr константой (начинается с '=')
                                valueToAdd =
                                    case String.uncons numStr of
                                        Just ('=', _) -> rawValue  -- ❗ Константа: оставляем как есть
                                        _ ->
                                            -- ❗ Обычная или непрямая адресация: ищем значение в `registers[rawValue]`
                                            case List.head (List.filter (\r -> r.number == rawValue) registers) of
                                                Just reg -> reg.value  -- Значение из регистра
                                                Nothing -> 0  -- Если регистра нет, используем 0

                            in
                            -- Найти значение в регистре 0
                            case List.head (List.filter (\r -> r.number == 0) registers) of
                                Just regZero ->
                                    -- ❗ Обновляем регистр 0, добавляя к нему `valueToAdd`
                                    let
                                        newValue = regZero.value * valueToAdd
                                        updatedRegisters = updateRegister 0 (String.fromInt newValue) registers
                                    in
                                    executeCommands rest updatedRegisters tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (0 :: changedRegisters)

                                Nothing ->
                                    -- Если регистра 0 нет (маловероятно), продолжаем выполнение
                                    executeCommands rest registers tape outputTape readIndex  allCommands   (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)          

                        Nothing ->
                            createErrorState "Chyba: nesprávny formát príkazu MUL. Použite MUL <číslo registra> , MUL *<číslo registra> alebo MUL =<konštanta>." currentStep registers tape outputTape readIndex (changedRegisters)
               
                ["div", numStr] ->
                    case resolveRegisterAddress numStr registers of
                        Just rawValue ->  
                            if rawValue == -1 then
                                -- ❌ Ошибка: регистра `numStr` нет в `registers`
                                createErrorState ("Chyba: register  " ++ numStr ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)
                            else
                            let
                                -- Проверяем, является ли numStr константой (начинается с '=')
                                valueToAdd =
                                    case String.uncons numStr of
                                        Just ('=', _) -> rawValue  -- ❗ Константа: оставляем как есть
                                        _ ->
                                            -- ❗ Обычная или непрямая адресация: ищем значение в `registers[rawValue]`
                                            case List.head (List.filter (\r -> r.number == rawValue) registers) of
                                                Just reg -> reg.value  -- Значение из регистра
                                                Nothing -> 0  -- Если регистра нет, используем 0

                            in
                            -- Найти значение в регистре 0
                            case List.head (List.filter (\r -> r.number == 0) registers) of
                                Just regZero ->
                                    -- ❗ Обновляем регистр 0, добавляя к нему `valueToAdd`
                                    let
                                        newValue = regZero.value // valueToAdd
                                        updatedRegisters = updateRegister 0 (String.fromInt newValue) registers
                                    in
                                    executeCommands rest updatedRegisters tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (0 :: changedRegisters)

                                Nothing ->
                                    -- Если регистра 0 нет (маловероятно), продолжаем выполнение
                                    executeCommands rest registers tape outputTape readIndex allCommands (currentStep +1)  isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)           

                        Nothing ->
                            createErrorState "Chyba: nesprávny formát príkazu DIV. Použite DIV <číslo registra> , DIV *<číslo registra> alebo DIV =<konštanta>." currentStep registers tape outputTape readIndex (changedRegisters)
                
                ["jump", label] ->
                    case findLabelPosition label allCommands of
                        Just (newStep, newCmds) ->
        
                            if isStepExecution then
                                executeCommands rest registers tape outputTape readIndex allCommands newStep isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)

                                -- ✅ Пошаговое выполнение: обновляем `currentStep`, но выполняем `rest`
                            else
                                        -- 🔄 Полное выполнение: выполняем `newCmds`
                                executeCommands newCmds registers tape outputTape readIndex allCommands (newStep+1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                            
                        Nothing ->
                            -- ❌ Если `label` не найден, просто продолжаем выполнение
                                createErrorState ("Chyba: label " ++ label ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)

                ["jzero", label] ->
                    case List.head (List.filter (\r -> r.number == 0) registers) of
                        Just reg ->

                            if reg.value == 0 then
                                case findLabelPosition label allCommands of
                                    Just (newStep, newCmds) ->
                                        let
                                            _=Debug.log "newStep"  newStep
                                        in
                                        
                                        if isStepExecution then
                                        -- ✅ Пошаговое выполнение: обновляем `currentStep`, но выполняем `rest`
                                            executeCommands rest registers tape outputTape readIndex allCommands newStep isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                                        else
                                        -- 🔄 Полное выполнение: выполняем `newCmds`
                                            executeCommands newCmds registers tape outputTape readIndex allCommands (newStep+1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                                    Nothing ->
                                        -- ❌ Если `label` не найден, просто продолжаем выполнение
                                        createErrorState ("Chyba: label " ++ label ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)
                            else
                                -- 🚀 Если в регистре 0 НЕ 0, просто продолжаем выполнение
                                executeCommands rest registers tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                        Nothing ->
                            -- ❌ Если регистра `0` вообще нет, продолжаем выполнение
                            executeCommands rest registers tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)

                ["jgtz", label] ->
                    case List.head (List.filter (\r -> r.number == 0) registers) of
                        Just reg ->
                            if reg.value > 0 then
                                case findLabelPosition label allCommands of
                                    Just (newStep, newCmds) ->
                                        let
                                            _=Debug.log "newStep"  newStep
                                        in
                                        if isStepExecution then
                                        -- ✅ Пошаговое выполнение: обновляем `currentStep`, но выполняем `rest`
                                            executeCommands rest registers tape outputTape readIndex allCommands newStep isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                                        else
                                                    -- 🔄 Полное выполнение: выполняем `newCmds`
                                            executeCommands newCmds registers tape outputTape readIndex allCommands (newStep+1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                                    Nothing ->
                                        createErrorState ("Chyba: label " ++ label ++ " neexistuje.") currentStep registers tape outputTape readIndex (changedRegisters)
                            else
                                executeCommands rest registers tape outputTape readIndex allCommands (currentStep +1) isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                        Nothing ->
                            executeCommands rest registers tape outputTape readIndex allCommands  (currentStep +1)  isStepExecution (amountOfExecutedCommands + 1) (changedRegisters)
                _ ->
                    if String.contains ":" cmd then
                        executeCommands rest registers tape outputTape readIndex allCommands (currentStep + 1) isStepExecution  amountOfExecutedCommands (changedRegisters)
                    else
                    createErrorState ("Chyba: nesprávny formát príkazu " ++ String.toUpper cmd ++ " .") currentStep registers tape outputTape readIndex (changedRegisters)
createErrorState : String -> Int -> List Register -> List String -> List String -> Int -> List Int-> ExecutionState
createErrorState errorMessage currentStep registers tape outputTape readIndex changedRegisters =
    { registers = registers
    , tape = tape
    , outputTape = outputTape
    , tapeReadIndex = 0
    , currentStep = -1
    , errorMessage = Just errorMessage
    , errorStep = Just currentStep
    , amountOfExecutedCommands = readIndex
    , changedRegisters = changedRegisters
    }

resolveRegisterAddress : String -> List Register -> Maybe Int
resolveRegisterAddress regStr registers =
    case String.uncons regStr of
        -- 🔹 Непрямая адресация (например, *5)
        Just ('*', rest) ->
            case String.toInt rest of
                Just indirectRegNum ->
                    case List.head (List.filter (\r -> r.number == indirectRegNum) registers) of
                        Just reg -> Just reg.value  -- ✅ Если регистр существует, возвращаем его значение
                        Nothing -> Just -1  -- ❌ Chyba: register  не существует, возвращаем `-1`
                Nothing -> Just -1  -- ❌ Ошибка парсинга числа, возвращаем `-1`
        
        -- 🔹 Константа (например, =100)
        Just ('=', rest) ->
            String.toInt rest  -- ✅ Просто конвертируем число
        
        -- 🔹 Прямая адресация (например, 5)
        _ ->
            case String.toInt regStr of
                Just regNum ->
                    if List.any (\r -> r.number == regNum) registers then
                        Just regNum  -- ✅ Регистр существует, возвращаем его номер
                    else
                        Just -1  -- ❌ Ошибка: регистра нет, возвращаем `-1`
                Nothing -> Just -1  -- ❌ Ошибка парсинга числа, возвращаем `-1`


findLabelPosition : String -> List String -> Maybe (Int, List String)
findLabelPosition label cmds =
    let
        labelWithColon = label ++ ":"
        indexedCmds = List.indexedMap Tuple.pair cmds  -- [(индекс, строка)]
    in
    case List.filter (\(i, cmd) -> cmd == labelWithColon) indexedCmds of
        (index, _) :: _ ->
            -- ✅ Возвращаем индекс метки и команды после неё
            Just (index, List.drop (index + 1) cmds)

        [] ->
            -- ❌ Если метка не найдена, возвращаем `Nothing`
            Nothing


-- Поиск метки и возврат списка после неё
findAfterLabel : String -> List String -> Maybe (List String)
findAfterLabel labelWithColon list =
    case list of
        [] -> Nothing
        x :: xs ->
            if String.toUpper x == labelWithColon then
                Just xs  -- Нашли метку, возвращаем всё после неё
            else
                findAfterLabel labelWithColon xs  -- Рекурсивный поиск

-- Сбор команд до следующей метки
collectCommands : List String -> List String
collectCommands list =
    case list of
        [] -> []
        x :: xs ->
            if String.contains ":" x then
                []  -- Остановиться на следующей метке
            else
                x :: collectCommands xs  -- Добавить команду и продолжить



removeComments : String -> String
removeComments code =
    code
        |> String.lines  -- ✅ Разбиваем на строки
        |> List.map (\line -> 
            case String.split "//" line of
                firstPart :: _ -> String.trim firstPart  -- ✅ Берём только код до `//`
                [] -> line
           )
        |> String.join "\n"  -- ✅ Объединяем строки обратно

parseLine : String -> List String
parseLine line =
    if String.startsWith "//" (String.trimLeft line) then
        []  -- ⛔ Строка — комментарий, не парсим
    else
    let
         -- ❗ Убираем пробелы после * или =
        cleanedLine = removeSpacesAfterSpecialChars line
        -- Приводим строку к нижнему регистру
        lineLower =
            String.toLower cleanedLine

        maybePattern =
            fromString "([a-zA-Z0-9_]+)\\s*:|(store|read|write|load|add|div|mul|sub|jgtz|jzero|jump|halt)(\\s*[=*]?\\s*[a-zA-Z0-9_]+)?"

        matches =
            case maybePattern of
                Just pattern ->
                    find pattern lineLower

                Nothing ->
                    []
    in
    List.map
        (\m ->
            case m.submatches of
                -- 3 подгруппы: [Maybe label, Maybe cmd, Maybe arg]
                [ Just label, Nothing, Nothing ] ->
                    -- Например, label = "next" => вернём "next:"
                    label ++ ":"

                [ Nothing, Just cmd, Just arg ] ->
                    -- Например, cmd = "jzero", arg = " next"
                    cmd ++ " " ++ String.trim arg

                [ Nothing, Just cmd, Nothing ] ->
                    -- Команда без аргумента (например, "mul")
                    cmd

                _ ->
                    -- Если ничего не совпало
                    m.match
        )
        matches


removeSpacesAfterSpecialChars : String -> String
removeSpacesAfterSpecialChars input =
    let
        -- Заменяем `* ` на `*` и `= ` на `=`
        withoutSpaces = 
            input
                |> String.replace " " ""
    in
    withoutSpaces

updateRegister : Int -> String -> List Register -> List Register
updateRegister regNum value registers =
    List.map
        (\reg ->
            if reg.number == regNum then
                { reg | value = String.toInt value |> Maybe.withDefault 0 }
            else
                reg
        )
        registers

getAllCommands : String -> List String
getAllCommands code =
    code
        |> String.split "\n"        -- Разделяем на строки
        |> List.map parseLine       -- Для каждой строки получаем список команд
        |> List.concat             -- Склеиваем всё в один список

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "wrapper"] 
        [
          div [ class "header-container" ]
            [ div [ class "header-title" ] [ text "Random Access Machine" ]
            , viewHelpModal model.showHelpModal
            , div [ class "controls" ]
                [ button [ class "control-button", onClick ExecuteAction ] [ text "Spustit" ]
                , button [ class "control-button" , onClick StopExecution] [ text "Zastavit" ]
                , button [ class "control-button" , onClick Step ] [ text "Krok" ]
                , button [ onClick ToggleHelpModal, class "control-button" ] [ text "?" ]
                ]
            , viewSlider model.sliderValue SliderChanged    
            , div [ class "import-export-buttons" ]
                [ button [ onClick ImportCode, class "import-button" ] [ text "Import code" ]
                , button [ onClick ExportCode, class "export-button" ] [ text "Export code" ]
                ]
            ]
        
        , div [ class "main-container" ]
            [ div [ class "tape-wrapper"] [viewTape model ],
              div [ class "code-container" ]
                [ 
                textarea
                        [ placeholder "Miesto pre napísanie kodu..."
                        , value model.code
                        , onInput UpdateCode
                        , class "code-area"
                        ]
                        []
                ]
            , div [ class "tables-wrapper" ]
                [ div [ class "register-container" ]
                    [ viewSecondTable model.code model.currentStep model.errorStep ],
                     div [ class "register-container" ]
                    [  viewError model.errorMessage ,
                    table []
                        (tableHeader :: List.map (\reg -> viewRegister reg model.previousRegisters model.registers) model.registers)
                    ]
                ]
            ,div [ class "bottom-info" ]
            [ div [ style "color" "black"]
                [ text ("Jednotková časová zložitosť: "
                        ++ String.fromInt (
                            if model.amountOfExecutedCommands > 0 then
                                model.amountOfExecutedCommands - 1
                            else
                                0
                        )
                    ) ]
            , div [ style "color" "black" ]
                [ text ("Jednotková priestorová zložitosť: " ++ String.fromInt (List.length (unique model.changedRegisters))) ]
            ]
            , div [ class "tape-wrapper-bottom"] [ 
                viewOutputTape model ]    
            ]
        ]

getCommands : String -> List String
getCommands code =
    code
        |> String.split "\n"                    -- разбиваем на строки
        |> List.map (String.split " ")          -- каждую строку разбиваем по пробелу
        |> List.concat                          -- склеиваем всё в один список
        |> List.filter (not << String.isEmpty)  -- убираем пустые строки (если где-то два пробела подряд)

viewSecondTable : String -> Int -> Maybe Int -> Html msg
viewSecondTable code currentStep errorStep =
    let
        _ = Debug.log "Rendering viewSecondTable: errorStep=" errorStep 
        allCommands = getAllCommands code
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Index" ]
                , th [] [ text "Label" ]
                , th [] [ text "Command" ]
                ]
            ]
        , tbody []
            (allCommands
                |> List.indexedMap
                    (\idx cmd ->
                        let
                            isActive = idx == currentStep
                            isError = errorStep == Just idx  -- ✅ Проверяем, строка с ошибкой или нет
                            rowStyle =
                                if isError then
                                    [ style "background-color" "red", style "color" "white" ] 
                                else if isActive then
                                    [ style "background-color" "lightgreen" ]  -- 🟡 Подсветка текущей команды
                                else
                                    []
                        in
                        tr (rowStyle ++ [ id ("row-" ++ String.fromInt idx) ])
                            [ td [] [ text (String.fromInt idx) ]
                            , td [] [ text (if String.contains ":" cmd then String.toUpper cmd else "") ]
                            , td [] [ text (if String.contains ":" cmd then "" else String.toUpper cmd) ]
                            ]
                    )
            )
        ]

        
tableHeader : Html Msg
tableHeader =
    tr []
        [ th [] [ text "Číslo registru" ]
        , th [] [ text "Hodnota registru" ]
        ]

viewSlider : Float -> (String -> Msg) -> Html Msg
viewSlider value msg =
    let
        speedLabels =
            [ "1", "2", "3", "4", "5" ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "width" "150px"
        ]
        [ div
            [ style "margin-bottom" "5px"
            , style "font-size" "20px"
            , style "color" "black"
            ]
            [ text "Rýchlosť" ]
        , input
            [ type_ "range"
            , Attr.min "1"
            , Attr.max "5"
            , step "1"
            , Attr.value (String.fromFloat value)
            , onInput msg
            , style "width" "100%"
            , style "background" "#ddd"
            , style "height" "6px"
            , style "border-radius" "5px"
            , style "outline" "none"
            , style "appearance" "none"
            ]
            []
        , div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "width" "100%"
            , style "margin-top" "10px"
            , style "font-size" "15px"
            , style "color" "WHITE"
            ]
            (List.map (text >> (\label -> div [] [ label ])) speedLabels)
        ]

viewSlider2 : Float -> (String -> Msg) -> Html Msg
viewSlider2 value msg =
    let
        speedLabels =
            [ "1", "2", "3", "4", "5" ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "width" "150px"
        ]
        [ div
            [ style "margin-bottom" "5px"
            , style "font-size" "20px"
            , style "color" "black"
            ]
            [ text "Rýchlosť" ]
        , input
            [ type_ "range"
            , Attr.min "1"
            , Attr.max "5"
            , step "1"
            , Attr.value (String.fromFloat value)
            , onInput msg
            , style "width" "100%"
            , style "background" "#ddd"
            , style "height" "6px"
            , style "border-radius" "5px"
            , style "outline" "none"
            , style "appearance" "none"
            ]
            []
        , div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "width" "100%"
            , style "margin-top" "10px"
            , style "font-size" "15px"
            , style "color" "black"
            ]
            (List.map (text >> (\label -> div [] [ label ])) speedLabels)
        ]

viewRegister : Register -> List Register -> List Register -> Html Msg
viewRegister reg prevRegisters allRegisters =
    let
        -- Определяем, сколько регистров изменилось
        changedRegisters =
            allRegisters
                |> List.filter (\r ->
                    case List.filter (\prev -> prev.number == r.number) prevRegisters of
                        [prev] -> prev.value /= r.value
                        _ -> False
                )

        -- Проверяем, изменился ли текущий регистр
        prevRegValue =
            prevRegisters
                |> List.filter (\r -> r.number == reg.number)
                |> List.head
                |> Maybe.map .value
                |> Maybe.withDefault reg.value

        isHighlighted =
            reg.value /= prevRegValue && List.length changedRegisters == 1  -- ✅ Подсвечиваем только если изменился ровно 1 регистр

        rowStyle =
            if isHighlighted then
                [ style "background-color" "lightgreen" ]
            else
                []

        rowId = id ("reg-" ++ String.fromInt reg.number)
    in
    tr (rowStyle ++ [rowId])
        [ td [] [ text (String.fromInt reg.number) ]
        , td [] [ text (String.fromInt reg.value) ]
        ]




viewTape : Model -> Html Msg
viewTape model =
    div [ class "tape-container" ]
        ([ button [ onClick AddTapeField, class "add-button" ] [ text "Pridať záznam" ] ]
        ++ List.indexedMap viewTapeField model.tape
        )

viewTapeField : Int -> String -> Html Msg
viewTapeField index value =
    div [ class "tape-field" ] 
        [ input
            [ type_ "text"
            , Html.Attributes.value value
            , onInput (\str ->
                if str == "" then
                    UpdateTapeField index ""  -- ⬅️ разрешаем очистку поля
                else if str == "-" then
                    UpdateTapeField index "-"
                else
                    case String.toInt str of
                        Just intVal ->
                            UpdateTapeField index (String.fromInt intVal)
                        Nothing ->
                            NoOp
              )
            , class "tape-input"
            ]
            []
        , button
            [ onClick (DeleteTapeField index)
            , class "delete-button"
            ]
            [ text "X" ]
        ]


viewOutputTape : Model -> Html Msg
viewOutputTape model =
    div [ class "tape-container" ]
        ( [ div [ class "add-button" ] [ text "Vystupna paska" ] ]
          ++ List.indexedMap viewOutputTapeField model.outputTape
        )

viewOutputTapeField : Int -> String -> Html Msg
viewOutputTapeField index value =
    div [ class "tape-field" ]
        [ input
            [ type_ "text"
            , Html.Attributes.value value
            , onInput (\str -> UpdateOutputTapeField index str)
            , class "tape-input"
            ]
            []
        ]

viewError : Maybe String -> Html Msg
viewError errorMsg =
    case errorMsg of
        Just msg ->
            div [ class "error-message" ]
                [ p [] [ text msg ] 
                , div []  
                    [ button [ onClick CloseError, class "error-close-button" ] [ text "Zatvoriť" ] ]
                ]
        Nothing ->
            text ""

viewHelpModal : Bool -> Html Msg
viewHelpModal show =
    if show then
        div [ class "modal-backdrop"
         ]
        
            [ div [ class "modal"
           ]
                [ h2 [] [ text "Ako používať Random Access Machine" ],
                 p [] [ text "Inštrukčná sada RAM" ]
                , table [ class "help-table" ]
               
                    [ thead []
                        [ tr []
                            [ th [] [ text "Inštrukcia" ]
                            , th [] [ text "Operand" ]
                            , th [] [ text "Popis" ]
                            ]
                        ]
                    , tbody []
                        [ tr []
                            [ td [] [ text "LOAD" ]
                            , td [] [ text "i, *i, =i" ]
                            , td [] [ text "operand sa načíta do akumulátora" ]
                            ]
                        , tr []
                            [ td [] [ text "STORE" ]
                            , td [] [ text "i, *i" ]
                            , td [] [ text "obsah akumulátora sa zapíše do pamäte podľa operandu" ]
                            ]
                        , tr []
                            [ td [] [ text "ADD" ]
                            , td [] [ text "i, *i, =i" ]
                            , td [] [ text "operand sa pričíta do akumulátora" ]
                            ]
                        , tr []
                            [ td [] [ text "SUB" ]
                            , td [] [ text "i, *i, =i" ]
                            , td [] [ text "operand sa odčíta od akumulátora" ]
                            ]
                         , tr []
                            [ td [] [ text "MUL" ]
                            , td [] [ text "i, *i, =i" ]
                            , td [] [ text "akumulátor sa prenásobí operandnom" ]
                            ]
                        , tr []
                            [ td [] [ text "DIV" ]
                            , td [] [ text "i, *i, =i" ]
                            , td [] [ text "akumulátor sa predelí operandom" ]
                            ] 
                        , tr []
                            [ td [] [ text "READ" ]
                            , td [] [ text "i, *i" ]
                            , td [] [ text "údaj zo vstupnej pásky sa zapíše do pamäte podľa operandu" ]
                            ]    
                        , tr []
                            [ td [] [ text "WRITE" ]
                            , td [] [ text "i, *i" ]
                            , td [] [ text "operand sa zapíše na výstupnú pásku" ]
                            ]       
                        , tr []
                            [ td [] [ text "JUMP" ]
                            , td [] [ text "návestie" ]
                            , td [] [ text "skok na návestie" ]
                            ]
                        , tr []
                            [ td [] [ text "JZERO" ]
                            , td [] [ text "návestie" ]
                            , td [] [ text "skok na návestie, ak akumulátor nulový" ]
                            ]
                        , tr []
                            [ td [] [ text "JGTZ" ]
                            , td [] [ text "návestie" ]
                            , td [] [ text "skok, ak akum. väčší ako nula" ]
                            ]
                        , tr []
                            [ td [] [ text "HALT" ]
                            , td [] [ text "" ]
                            , td [] [ text "zastavenie programu" ]
                            ]
                        ]
                    ]
                
                , span [style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-bottom" "10px",style "margin-top" "10px"] 
                [ text " *Návestie má byť vo formáte‚ 'slovo' + ':', napr. skok_sem:" ]

                , span [style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-bottom" "10px",style "margin-top" "10px"] 
                [ text " *Každý príkaz musí byť na samostatnom riadku" ]
                 
                , div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-bottom" "10px",style "margin-top" "10px" ]
                    [ button [ class "import-button" ] [ text "Import code" ]
                    , span [] [ text "– slúži na načítanie kódu zo súboru vo formáte .txt do priestoru na písanie kódu" ]
                    ]
                , div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-bottom" "10px" ]
                    [ button [ class "export-button" ] [ text "Export code" ]
                    , span [] [ text "– slúži na ukladanie kódu z priestoru na písanie kódu do súboru vo formáte .txt" ]
                    ] 
                , div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-bottom" "10px",style "margin-top" "10px"  ]
                    [ button [ class "control-button" ] [ text "Krok" ]
                    , span [] [ text "– slúži na krokové spúšťanie kódu, jedno stlačenie vykoná jeden príkaz" ]
                    ]   
                , div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-top" "10px" ]
                    [ button [ class "control-button" ] [ text "Spustit" ]
                    , span [] [ text "– slúži na spustenie kódu" ]
                    ]           
                , div
                    [ style "margin-top" "20px"
                    , style "color" "black"
                    ]
                    [ div
                        [ style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "10px"
                        ]
                        [ viewSlider2 5 IgnoredSliderChange
                        , span [] [ text "– slúži na nastavenie rýchlosti vykonávania programu." ]
                        ]
                    , ul [ style "margin-top" "10px", style "margin-left" "20px" ]
                        [ li [] [ text "Rýchlosť 1 – 1 príkaz za 1 sekundu" ]
                        , li [] [ text "Rýchlosť 2 – 1 príkaz za 0,7 sekundy" ]
                        , li [] [ text "Rýchlosť 3 – 1 príkaz za 0,5 sekundy" ]
                        , li [] [ text "Rýchlosť 4 – 1 príkaz za 0,3 sekundy" ]
                        , li [] [ text "Rýchlosť 5 – okamžité vykonanie všetkých príkazov" ]
                        ]
                    ]
                
                
                , div [ style "display" "flex", style "justify-content" "flex-end" ]
                    [ button [ onClick ToggleHelpModal, class "close-button" ] [ text "Zavrieť" ] ]]


                   
            ]
    else
        text ""

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

