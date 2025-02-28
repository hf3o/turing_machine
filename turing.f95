program turing_machine
    implicit none
    integer, parameter :: tape_size = 100, max_rules = 50
    character(len=1) :: tape(tape_size)
    integer :: head, state, i, num_rules
    logical :: running

    ! Struktura przechowująca reguły
    integer :: states(max_rules), symbols(max_rules)
    integer :: new_symbols(max_rules), new_states(max_rules)
    character(len=1) :: moves(max_rules)

    ! Inicjalizacja taśmy
    tape = '0'
    head = tape_size / 2  ! Środek taśmy
    state = 1             ! Stan początkowy (A = 1)
    running = .true.

    ! Wczytujemy reguły
    call load_rules(states, symbols, new_symbols, moves, new_states, num_rules)

    print *, "Start maszyny Turinga..."

    ! Główna pętla symulacji
    do while (running)
        call step_machine(tape, head, state, running, states, symbols, new_symbols, moves, new_states, num_rules)
        call print_tape(tape, head)
    end do

    print *, "Maszyna zatrzymała się!"
    print *, "Stan końcowy taśmy:"
    call print_tape(tape, -1)  ! Ostateczny stan taśmy

contains

    ! Wczytuje reguły z pliku
    subroutine load_rules(states, symbols, new_symbols, moves, new_states, num_rules)
        integer, dimension(max_rules), intent(out) :: states, symbols, new_symbols, new_states
        character(len=1), dimension(max_rules), intent(out) :: moves
        integer, intent(out) :: num_rules
        integer :: i
        open(10, file="rules.txt", status="old", action="read")
        num_rules = 0
        do i = 1, max_rules
            read(10, *, end=100) states(i), symbols(i), new_symbols(i), moves(i), new_states(i)
            num_rules = num_rules + 1
        end do
100     close(10)
        print *, "Wczytano ", num_rules, " reguł."
    end subroutine load_rules

    ! Pojedynczy krok maszyny Turinga
    subroutine step_machine(tape, head, state, running, states, symbols, new_symbols, moves, new_states, num_rules)
        character(len=1), dimension(tape_size), intent(inout) :: tape
        integer, intent(inout) :: head, state
        logical, intent(inout) :: running
        integer, dimension(max_rules), intent(in) :: states, symbols, new_symbols, new_states
        character(len=1), dimension(max_rules), intent(in) :: moves
        integer, intent(in) :: num_rules
        integer :: i
        logical :: found

        found = .false.
        do i = 1, num_rules
            if (states(i) == state .and. symbols(i) == ichar(tape(head)) - ichar('0')) then
                tape(head) = char(ichar('0') + new_symbols(i))
                if (moves(i) == 'L') then
                    head = head - 1
                else if (moves(i) == 'R') then
                    head = head + 1
                end if
                state = new_states(i)
                found = .true.
                exit
            end if
        end do

        ! Jeśli nie znaleziono reguły, zatrzymujemy maszynę
        if (.not. found) then
            print *, "Błąd: brak reguły dla stanu", state, "i symbolu", tape(head)
            running = .false.
        end if

        ! Sprawdzamy, czy głowica nie wyszła poza taśmę
        if (head < 1 .or. head > tape_size) then
            print *, "Błąd: głowica wyszła poza zakres taśmy!"
            running = .false.
        end if
    end subroutine step_machine

    ! Wizualizacja taśmy
    subroutine print_tape(tape, head)
        character(len=1), dimension(tape_size), intent(in) :: tape
        integer, intent(in) :: head
        integer :: i

        print '(A)', "Taśma: "
        do i = 1, tape_size
            if (i == head) then
                write(*, '(A)', advance='no') "[" // tape(i) // "]"
            else
                write(*, '(A)', advance='no') " " // tape(i) // " "
            end if
        end do
        print *  ! Nowa linia
    end subroutine print_tape

end program turing_machine

