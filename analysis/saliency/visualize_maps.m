function [saved_img, saliency_data] = compare_saliency(img, left_rect, right_rect, saliency_map, title_text, show_fig)
%COMPARE_SALIENCY  Target/distractor saliency for a two-image composite.
%   Measures saliency inside a tight box around each object. LEFT_RECT and
%   RIGHT_RECT are the [x y w h] pasted-image rectangles returned by
%   make_pair_canvas (pos1, pos2). The object box is refined inside each
%   rectangle by sampling that rectangle's OWN background colour from its
%   corners, so it works whether the background is light gray, white, etc.
%
%   [saved_img, saliency_data] = compare_saliency(img, left_rect, right_rect)
%   [...] = compare_saliency(img, left_rect, right_rect, saliency_map, title_text)
 
    if nargin < 6 || isempty(show_fig)
        show_fig = true;
    end
    if nargin < 5 || isempty(title_text)
        title_text = "Saliency Map";
    end
    if nargin < 4 || isempty(saliency_map)
        out_gbvs = gbvs(img);
        saliency_map = out_gbvs.master_map_resized;
    else
        saliency_map = imresize(saliency_map, [size(img, 1), size(img, 2)]);
    end
 
    % Refine each placement rectangle to a tight box around the object.
    left_bbox  = object_bbox(img, left_rect);
    right_bbox = object_bbox(img, right_rect);
 
    [mean_l, max_l, sd_l] = extract_saliency(saliency_map, left_bbox);
    [mean_r, max_r, sd_r] = extract_saliency(saliency_map, right_bbox);
 
    saliency_data = table(mean_l, max_l, sd_l, mean_r, max_r, sd_r, ...
        'VariableNames', {'MeanTargetSaliency', 'MaxTargetSaliency', 'SDTargetSaliency', ...
                          'MeanDistractorSaliency', 'MaxDistractorSaliency', 'SDDistractorSaliency'});
 
    if show_fig
        saved_img = draw_bboxes(img, saliency_map, left_bbox, right_bbox, saliency_data, title_text);
    else
        saved_img = [];
    end
end
 
 
function bbox = object_bbox(img, cell_rect, tol)
%OBJECT_BBOX  Tight [x y w h] box around the object inside a known rectangle.
%   Samples the rectangle's background colour from its four corners and keeps
%   everything that differs from it. Handles light-gray OR white (or any
%   roughly-uniform) backgrounds automatically. Assumes the object is roughly
%   centred so the corners are background.
    if nargin < 3 || isempty(tol)
        tol = 25;   % colour distance 0-255. Raise if bg leaks in, lower if object clips.
    end
 
    x = round(cell_rect(1)); y = round(cell_rect(2));
    w = round(cell_rect(3)); h = round(cell_rect(4));
 
    % Clamp the rectangle to the image bounds.
    [H, W, ~] = size(img);
    x = max(1, x); y = max(1, y);
    w = min(w, W - x + 1); h = min(h, H - y + 1);
 
    sub = double(img(y:y+h-1, x:x+w-1, :));
 
    % Background colour = median of the four corner patches.
    k = max(3, round(0.05 * min(h, w)));
    corners = [reshape(sub(1:k,         1:k,         :), [], 3);
               reshape(sub(1:k,         end-k+1:end, :), [], 3);
               reshape(sub(end-k+1:end, 1:k,         :), [], 3);
               reshape(sub(end-k+1:end, end-k+1:end, :), [], 3)];
    bg = median(corners, 1);
 
    % Foreground = pixels that differ from the background colour.
    dist = sqrt(sum((sub - reshape(bg, 1, 1, 3)).^2, 3));
    fg = dist > tol;
 
    % Clean up: drop specks, bridge gaps (e.g. white diaper to skin), fill holes.
    fg = bwareaopen(fg, round(5e-4 * w * h));
    fg = imclose(fg, strel('disk', 7));
    fg = imfill(fg, 'holes');
 
    [rr, cc] = find(fg);
    if isempty(rr)
        bbox = [x, y, w, h];   % fallback: whole rectangle
        return
    end
    bbox = [x + min(cc) - 1, y + min(rr) - 1, ...
            max(cc) - min(cc) + 1, max(rr) - min(rr) + 1];
end
 
 
function [mean_s, max_s, sd_s] = extract_saliency(saliency_map, bbox)
%EXTRACT_SALIENCY  Mean/max/SD of the saliency map inside an [x y w h] box.
    [H, W] = size(saliency_map);
    x1 = max(1, round(bbox(1)));
    y1 = max(1, round(bbox(2)));
    x2 = min(W, round(bbox(1) + bbox(3) - 1));
    y2 = min(H, round(bbox(2) + bbox(4) - 1));
 
    region = saliency_map(y1:y2, x1:x2);
    mean_s = mean(region(:));
    max_s  = max(region(:));
    sd_s   = std(region(:));
end
 
 
function saved_image = draw_bboxes(img, saliency_map, left_bbox, right_bbox, saliency_data, title_text)
%DRAW_BBOXES  Overlay heatmap + boxes + labels and capture the figure as an image.
    if nargin < 6
        title_text = "Saliency Map";
    end
    fig = figure;
    hm = heatmap_overlay(img, saliency_map);
    imshow(hm, []);
    set(gca, 'Position', [0, 0, 1, 0.8]);
    hold on;
 
    text(left_bbox(1) + left_bbox(3)/2, max(40, left_bbox(2) - 150), ...
        sprintf('Max: %.3f | Mean: %.3f', saliency_data.MaxTargetSaliency, saliency_data.MeanTargetSaliency), ...
        'Color', 'black', 'FontSize', 30, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    text(right_bbox(1) + right_bbox(3)/2, max(40, right_bbox(2) - 150), ...
        sprintf('Max: %.3f | Mean: %.3f', saliency_data.MaxDistractorSaliency, saliency_data.MeanDistractorSaliency), ...
        'Color', 'black', 'FontSize', 30, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    title(sprintf('%s | Mean Salience Diff: %.4f', title_text, ...
        saliency_data.MeanTargetSaliency - saliency_data.MeanDistractorSaliency), 'FontSize', 40);
 
    rectangle('Position', left_bbox,  'EdgeColor', 'r', 'LineWidth', 2);
    rectangle('Position', right_bbox, 'EdgeColor', 'b', 'LineWidth', 2);
 
    frame = getframe(fig);
    [saved_image, ~] = frame2im(frame);
end


function cell = fit_in_cell(im, cellH, cellW, grayVal)
    if ismatrix(im); im = repmat(im, [1 1 3]); else; im = im(:, :, 1:3); end
    if ~isa(im, 'uint8'); im = im2uint8(im); end

    [h, w, ~] = size(im);
    s = min(cellW / w, cellH / h);     % scale to fit, aspect preserved
    im = imresize(im, s);

    [h, w, ~] = size(im);
    cell = uint8(grayVal * ones(cellH, cellW, 3));
    r0 = floor((cellH - h) / 2);
    c0 = floor((cellW - w) / 2);
    cell(r0 + (1:h), c0 + (1:w), :) = im;
end

function [canvas, pos1, pos2] = make_pair_canvas(img1, img2, opts)
    arguments
        img1
        img2
        opts.CanvasW (1,1) double = 1920
        opts.CanvasH (1,1) double = 1080   % 16:9
        opts.CellW   (1,1) double = 600
        opts.CellH   (1,1) double = 600
        opts.Margin  (1,1) double = 120
        opts.Gray    (1,1) double = 128
    end

    canvas = uint8(opts.Gray * ones(opts.CanvasH, opts.CanvasW, 3));

    img1 = fit_in_cell(img1, opts.CellH, opts.CellW, opts.Gray);
    img2 = fit_in_cell(img2, opts.CellH, opts.CellW, opts.Gray);

    rowTop = round((opts.CanvasH - opts.CellH)/2);   % vertically centered
    r = rowTop + (1:opts.CellH);

    c1 = opts.Margin + (1:opts.CellW);                               % center-left
    c2 = opts.CanvasW - opts.Margin - opts.CellW + (1:opts.CellW);   % center-right

    canvas(r, c1, :) = img1;
    canvas(r, c2, :) = img2;

    pos1 = [c1(1) r(1) opts.CellW opts.CellH];   % [x y w h]
    pos2 = [c2(1) r(1) opts.CellW opts.CellH];
end

function out = to_rgb(in)
    if ismatrix(in)              % grayscale -> 3 channels
        out = repmat(in, [1 1 3]);
    else
        out = in(:, :, 1:3);     % drop alpha if present
    end
    if ~isa(out, 'uint8'); out = im2uint8(out); end
end

% Create output table to store saliency metrics
% Read PROJECT_PATH from environment file
D = loadenv(fullfile(here, '..', '..', '.env'));
PROJECT_PATH = D("PROJECT_PATH");
disp(PROJECT_PATH);

% Read image pairs data
% Read image pairs data
image_pairs = readtable(fullfile(PROJECT_PATH, 'data/peekbank_stimuli.csv'));

% Create output table for saliency metrics
metrics = {'ImagePair', 'MeanTargetSaliency', 'MaxTargetSaliency', 'SDTargetSaliency', ...
           'MeanDistractorSaliency', 'MaxDistractorSaliency', 'SDDistractorSaliency'};

saliency_metrics = table('Size', [height(image_pairs), 8], ...
    'VariableTypes', {'string', 'double', 'double', 'double', 'double', 'double', 'double', 'double'}, ...
    'VariableNames', [metrics, {'MeanSaliencyDiff'}]);

comp_dir = fullfile(PROJECT_PATH, 'data', 'composites');
if ~exist(comp_dir, 'dir'); mkdir(comp_dir); end

display_every = 20;   % only show a figure every Nth pair
save_every    = 20;   % checkpoint the CSV every Nth pair
output_file   = fullfile(PROJECT_PATH, 'data', 'metadata', 'level-imagepair_added-saliency_data.csv');

for i = 1:height(image_pairs)
    img_pair = strcat(image_pairs.unique_pair{i});
    img1 = imread(fullfile(PROJECT_PATH, 'data', strcat(image_pairs.image1{i})));
    img2 = imread(fullfile(PROJECT_PATH, 'data', strcat(image_pairs.image2{i})));

    [img, pos1, pos2] = make_pair_canvas(img1, img2);
    imwrite(img, fullfile(comp_dir, string(img_pair) + ".png"));

    left_bbox  = object_bbox(img, pos1);
    right_bbox = object_bbox(img, pos2);

    show_fig = (mod(i, display_every) == 0);
    [~, saliency_data] = compare_saliency(img, left_bbox, right_bbox, [], [], show_fig);

    saliency_metrics.ImagePair(i) = string(img_pair);
    for m = metrics(2:end)
        saliency_metrics.(m{1})(i) = round(saliency_data.(m{1}), 4);
    end
    saliency_metrics.MeanSaliencyDiff(i) = round(saliency_metrics.MeanTargetSaliency(i) - saliency_metrics.MeanDistractorSaliency(i), 4);

    % Checkpoint so progress survives an interrupted run
    if mod(i, save_every) == 0
        writetable(saliency_metrics, output_file);
        fprintf('Saved progress after %d/%d image pairs\n', i, height(image_pairs));
    end
end

% Final save for the remaining rows
writetable(saliency_metrics, output_file);

% broken down saliency maps 
% img = imread("/path/to/image")
%[saved_img] = compare_saliency(img, out_gbvs.master_map_resized);
%[saved_img_dklcolor] = compare_saliency(img, out_gbvs.top_level_feat_maps{1,1}, "DKL Color map");
%[saved_img_intensity] = compare_saliency(img, out_gbvs.top_level_feat_maps{1,2}, "Intensity map");
%[saved_img_orientation] = compare_saliency(img, out_gbvs.top_level_feat_maps{1,3}, "Orientation map");
%close all;
%concatenated = cat(1, saved_img, saved_img_dklcolor, saved_img_intensity, saved_img_orientation);
%imshow(concatenated)