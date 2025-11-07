% Plotting and data visualization in MATLAB

fprintf('\n=== Plotting in MATLAB ===\n\n');

% Generate data
x = linspace(0, 2*pi, 100);
y1 = sin(x);
y2 = cos(x);

% Create figure
figure('Visible', 'off'); % Off for script mode

% Plot data
plot(x, y1, 'r-', 'LineWidth', 2);
hold on;
plot(x, y2, 'b--', 'LineWidth', 2);
hold off;

% Labels and title
xlabel('x');
ylabel('y');
title('Sine and Cosine Functions');
legend('sin(x)', 'cos(x)');
grid on;

% Save figure
saveas(gcf, '../tmp/plot.png');
fprintf('Plot saved to ../tmp/plot.png\n');

% Statistics
fprintf('\nStatistics:\n');
fprintf('Max sin(x): %.4f\n', max(y1));
fprintf('Min sin(x): %.4f\n', min(y1));
fprintf('Mean sin(x): %.4f\n', mean(y1));
fprintf('Std dev sin(x): %.4f\n', std(y1));

close all;
