import sys


def check(number):
    if number == 1:
        return False
    elif number % 2 == 0:
        return number / 2
    else:
        return (number * 3) + 1


def printf(format, *args):
    sys.stdout.write(format % args)


if __name__ == '__main__':
    # current_number = 1
    current_number = 2 ** 69

    while True:
        iterations = 0
        current_iteration = check(current_number)

        while current_iteration:
            current_iteration = check(current_iteration)
            iterations = iterations + 1

        if iterations > 1000:
            exit
        else:
            printf("%s:%d ", current_number, iterations)

        if current_number % 10 == 0:
            printf("\n")

        current_number = current_number + 1

# < ?php
#
#     // 3
#     x + 1
#     $current_number = 1;
#     // $current_number = pow(2, 68);
#     // printf("POW(%s)\n",$current_number);
#     // exit;
#
#     while (true){
#     $iterations = 0;
#     $current_iteration = check($current_number);
#
#     while ($current_iteration){
#     $current_iteration = check($current_iteration);
#     $iterations++;
#     }
#
#     if ($iterations > 1000){
#     exit;
#     } else {
#     printf("%s:%d ", $current_number, $iterations);
#     if ($current_number % 20 == 0){
#     printf("\n");
#     }
#     }
#
#
#     $current_number + +;
# }
#
# function
# check($number){
# if ($number == 1){
# return FALSE;
# } elseif($number % 2 == 0){
# return $number / 2;
# } else {
# return ($number * 3) + 1;
# }
# }
