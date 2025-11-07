% Array operations in MATLAB

fprintf('\n=== Array Operations in MATLAB ===\n\n');

% Create arrays
numbers = [1, 2, 3, 4, 5];
matrix = [1, 2, 3; 4, 5, 6; 7, 8, 9];

fprintf('Numbers: ');
disp(numbers);

fprintf('Matrix:\n');
disp(matrix);

% Array length
fprintf('Length: %d\n', length(numbers));
fprintf('Size: %dx%d\n', size(matrix, 1), size(matrix, 2));

% Element access
fprintf('\nFirst element: %d\n', numbers(1));
fprintf('Last element: %d\n', numbers(end));

% Array operations
fprintf('\nSquare (element-wise): ');
disp(numbers .^ 2);

fprintf('Sum: %d\n', sum(numbers));
fprintf('Product: %d\n', prod(numbers));
fprintf('Mean: %.2f\n', mean(numbers));
fprintf('Max: %d\n', max(numbers));
fprintf('Min: %d\n', min(numbers));

% Matrix operations
fprintf('\nMatrix sum: %d\n', sum(matrix, 'all'));
fprintf('Matrix transpose:\n');
disp(matrix');

% Linspace and logspace
fprintf('Linspace 0-10 (5 points): ');
disp(linspace(0, 10, 5));

% Find elements
fprintf('\nFind elements > 3: ');
disp(find(numbers > 3));

% Logical indexing
fprintf('Numbers > 3: ');
disp(numbers(numbers > 3));
