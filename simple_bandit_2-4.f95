program SimpleBandit

implicit none

! This program is an implementation of the section 2.4 simple bandit pseudocode from the book:
! "Reinforcement Learning: An Introduction" by Richard S. Sutton and Andrew G. Barto

!-------------------- * Declaring the variables * ------------------------

real,allocatable,dimension(:) :: Q, N
integer :: i, n_steps, k, A
real :: epsilon, reward


!-------------------- * Initialising the variables * ------------------------

! The number of actions that can be chosen
k = 4

! The number of times to chose an action:
n_steps = 1000

! The epsilon value for the epsilon greedy algorithm
epsilon = 0.9

! The estimated action value for an action:
allocate(Q(k))

! The number of times that action has been chosen:
allocate(N(k))

do i = 1, k
    Q(i) = 0
    N(i) = 0
end do

!-------------------- * Main bandit loop * ------------------------

do i = 1, n_steps
    call chose_action_eps_greedy(Q, k, epsilon, A)
    print *, A
    call calculate_reward(A, reward)

    !Update N and Q
    N(A) = N(A) + 1
    Q(A) = Q(A) + 1/(N(A)) * (reward - Q(A))

end do


end program

!-------------------- * Subroutines * ------------------------

subroutine chose_action_eps_greedy(Q, k, epsilon, A)
    implicit none

    integer, intent(in) :: k
    real, intent(in) :: epsilon
    real, dimension(k), intent(in) :: Q
    integer, intent(out) :: A
    real :: rand_n1, rand_n2, max_Q
    integer :: ii

    ! Generating a random number
    call random_number(rand_n1)

    ! If the number is smaller than epsilon chose the action with larger Q (Exploit)
    if (rand_n1 <= epsilon) then

        A = 0
        max_Q = 0

        ! Find which action has the largest Q
        do ii = 1, k
            if (max_Q < Q(ii)) then
                max_Q = Q(ii)
                A = ii
            end if
        end do

    ! Otherwise, choose a random action (Explore)
    else if (rand_n1 > epsilon) then
        call random_number(rand_n2)

        A = 1 + FLOOR((k)*rand_n2)
    end if


end subroutine


subroutine calculate_reward(A, reward)

    integer, intent(in) :: A
    real, intent(out) :: reward
    real :: rand_noise

    call random_number(rand_noise)
    reward = A**2.0 + rand_noise*20

end subroutine
