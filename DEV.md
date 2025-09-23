### Настройка окружения

Далее инструкции по найстройки всего под GNU/Linux.
Но на Windows+WSL2 тоже должно работать.

Во-первых, нужен пакетный менеджер opam версии 2.х.
Системный OCaml (установленный, например, из репозиториев Ubuntu) использовать не рекомендуется.
После установки opam (>= 2.1) следует его проинициализировать и установить правильный компилятор (OCaml 4.14.2).

На данный момент нужен:

    opam init --bare
    opam update
    opam switch create fp25 --packages=ocaml-variants.4.14.2+options,ocaml-option-flambda --yes

Если что-то пошло не так, то стоит удалить `~/.opam` и попробовать заново

После установки у вас будет рабочий компилятор по-умолчанию в директории `~/.opam/fp25/bin`.

В конце установки opam вам предложит что-то добавить в `~/.bashrc`, чтобы пути к компилятору автоматически подхватывались.

Надежнее запариться утилитой [direnv](https://ocaml.org/docs/opam-path#using-direnv), чтобы она сама проставляла нужные пути, при переходу в директорию с домашкой.
Выше название switch `fp25` не совсем с потолка, потому что оно прописано в `.envrc`, связанным с утилитой `direnv`.

Если что-то пошло не так, то всегда можно указать нужный свитч руками командой, например:

    export OPAMSWITCH=fp25 && eval $(opam env)

и затем убедиться, что путь до компилятора правильный

    $ which ocamlopt
    /home/username/.opam/fp25/bin/ocamlopt

Зависимости для демо-проекта (для не-Ubuntu может понадобится что-то дополнительное):

    opam install ./Lambda ocaml-lsp-server --deps-only --with-test --yes

#### VsCode

В процессе работы вам также понадобится пакеты из opam в том числе для разработки в VsCode.
Скорее всего необходимый минимум установится с помощью `make deps`

     $ which ocamlformat
     /home/username/.opam/fp25/bin/ocamlformat

Когда вы будете запускать VsCode, то информация об  окружении opam из файла `~/.bashrc` автоматически применяться не будет, потому что так это работает в UNIX системах из покон веков.
Чтобы облегчить себе возню с окружением, рекомендуется пользоваться утилитой `direnv`.
Подробнее читать [here](https://ocaml.org/docs/opam-path#using-direnv).

Если `direnv` пока не установили, но хочется попробовать VsCode, то нужно его запускать из-под opam командой `opam exec -- code`, либо прописать в месте запуска правильную переменную среды OPAMSWITCH, и запускать opam через sh: `sh -c 'eval $(opam env) && code'`

Когда VsCode запустится, её плагин https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform слева снизу должен показать, что правильная версия компилятора подцепилась.

![alt text](https://github.com/Kakadu/fp2025/blob/master/vscode.png?raw=true)


Необходимо также в VsCode включить автоформатирование: `Settings`->`Text Editor`->`Formatting`->`Format On Paste` и `Format on Save`.

