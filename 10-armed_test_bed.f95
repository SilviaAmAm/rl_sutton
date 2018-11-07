program TestBed
    implicit none

    ! This program is an implementation of the section 2.3 armed bandit test bed from the book:
    ! "Reinforcement Learning: An Introduction" by Richard S. Sutton and Andrew G. Barto

    ! Generate 2000 k-armed bandit problems with k = 10

    ! The real action values are chosen from a Gaussian distribution with mean 0 and variance 1
    ! q*(a) = N(0, 1) with a = 1, .., 10

    ! When a learning method choses the action At at time t, the reward is chosen from a Gaussian with mean q*(At) and variance 1
    ! Rt = N(q*(At), 1)

    ! Each learning method learns for 1000 time steps, and this is repeated for 2000 times

    !-------------------- * Declaring the variables * ------------------------

    real,allocatable,dimension(:) :: q_star, reward_given
    integer :: i, j,  n_steps, n_test, k, A
    real :: epsilon, reward


    !-------------------- * Initialising the variables * ------------------------

    ! The number of actions that can be chosen
    k = 10

    ! The number of times to chose an action:
    n_steps = 1000
    n_test = 2000

    ! The epsilon value for the epsilon greedy algorithm
    epsilon = 0.9

    ! The estimated action value for an action:
    allocate(q_star(k))

    ! Initialising the array to store the results
    allocate(reward_given(n_steps))

    do j=1,n_steps
        reward_given(j) = 0
    end do

    ! Initialising the values of q_star
    call initialise_action_value(q_star, k)

    !-------------------- * Main bandit loop * ------------------------
    do k = 1, n_test
        do i = 1, n_steps

            ! TODO need to modify so that they use sampled averages for the estimate of qstar
            call chose_action_eps_greedy(q_star, k, epsilon, A)
            call calculate_reward(A, k, q_star, reward)

            reward_given(i) = reward_given(i) + reward

        end do
    end do

    open(21,file="results.csv")

    do j = 1, n_steps
        reward_given(j) = reward_given(j) / n_test
        write(21, *) j,",", reward_given(j)
    end do

    close(21)


end program

!-------------------- * Subroutines * ------------------------

! This subroutine initialises the action values for each of the actions from a Gaussian distribution of mean 0 and
! variance 1
subroutine initialise_action_value(action_val, n_actions)
    implicit none

    integer, intent(in) :: n_actions
    real, dimension(n_actions), intent(out) :: action_val
    integer :: i
    real :: r1, r2
    real, parameter ::  pi=3.14159265359

    do i = 1, n_actions
        ! Uniform random numbers
        call random_number(r1)
        call random_number(r2)

        !Turning them into gaussian sampled (mean 0 and std 1)
        action_val(i) = SQRT(-2.0 * LOG(r1)) * COS(2 * pi * r2)

    end do
end subroutine

! This subroutine uses the epsilon greedy algorithm to chose the next action
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


! This subroutine calculates the reward for each action chosen
subroutine calculate_reward(A, k, action_value, reward)
    implicit none

    integer, intent(in) :: A, k
    real, dimension(k), intent(in) :: action_value
    real, intent(out) :: reward
    real :: r1, r2, g1
    real, parameter ::  pi=3.14159265359

    ! Uniform random numbers
    call random_number(r1)
    call random_number(r2)

    !Turning it into gaussian sampled (mean 0 and std 1)
    g1 = SQRT(-2.0 * LOG(r1)) * COS(2 * pi * r2)

    ! Turning it into gaussian sampled (mean action_value, std 1)
    reward = g1 + action_value(A)

end subroutine
