# Generated by Mochi compiler v0.10.27 on 2025-07-17T06:41:19Z
require 'ostruct'

def _avg(v)
  list = nil
  if defined?(MGroup) && v.is_a?(MGroup)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  list.sum(0.0) / list.length
end
def _json(v)
  require 'json'
  obj = v
  if v.is_a?(Array)
    obj = v.map { |it| it.respond_to?(:to_h) ? it.to_h : it }
  elsif v.respond_to?(:to_h)
    obj = v.to_h
  end
  puts(JSON.generate(obj))
end

$store_sales = [OpenStruct.new(ss_quantity: 5, ss_ext_discount_amt: 5.0, ss_net_paid: 7.0), OpenStruct.new(ss_quantity: 30, ss_ext_discount_amt: 10.0, ss_net_paid: 15.0), OpenStruct.new(ss_quantity: 50, ss_ext_discount_amt: 20.0, ss_net_paid: 30.0), OpenStruct.new(ss_quantity: 70, ss_ext_discount_amt: 25.0, ss_net_paid: 35.0), OpenStruct.new(ss_quantity: 90, ss_ext_discount_amt: 40.0, ss_net_paid: 50.0)]
$reason = [OpenStruct.new(r_reason_sk: 1)]
$bucket1 = (((((($store_sales)).select { |s| ((s.ss_quantity >= 1) && (s.ss_quantity <= 20)) }).map { |s| s }).length > 10) ? _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 1) && (s.ss_quantity <= 20)) }).map { |s| s.ss_ext_discount_amt }) : _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 1) && (s.ss_quantity <= 20)) }).map { |s| s.ss_net_paid }))
$bucket2 = (((((($store_sales)).select { |s| ((s.ss_quantity >= 21) && (s.ss_quantity <= 40)) }).map { |s| s }).length > 20) ? _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 21) && (s.ss_quantity <= 40)) }).map { |s| s.ss_ext_discount_amt }) : _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 21) && (s.ss_quantity <= 40)) }).map { |s| s.ss_net_paid }))
$bucket3 = (((((($store_sales)).select { |s| ((s.ss_quantity >= 41) && (s.ss_quantity <= 60)) }).map { |s| s }).length > 30) ? _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 41) && (s.ss_quantity <= 60)) }).map { |s| s.ss_ext_discount_amt }) : _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 41) && (s.ss_quantity <= 60)) }).map { |s| s.ss_net_paid }))
$bucket4 = (((((($store_sales)).select { |s| ((s.ss_quantity >= 61) && (s.ss_quantity <= 80)) }).map { |s| s }).length > 40) ? _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 61) && (s.ss_quantity <= 80)) }).map { |s| s.ss_ext_discount_amt }) : _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 61) && (s.ss_quantity <= 80)) }).map { |s| s.ss_net_paid }))
$bucket5 = (((((($store_sales)).select { |s| ((s.ss_quantity >= 81) && (s.ss_quantity <= 100)) }).map { |s| s }).length > 50) ? _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 81) && (s.ss_quantity <= 100)) }).map { |s| s.ss_ext_discount_amt }) : _avg(((($store_sales)).select { |s| ((s.ss_quantity >= 81) && (s.ss_quantity <= 100)) }).map { |s| s.ss_net_paid }))
$result = ((($reason)).select { |r| (r.r_reason_sk == 1) }).map { |r| OpenStruct.new(bucket1: $bucket1, bucket2: $bucket2, bucket3: $bucket3, bucket4: $bucket4, bucket5: $bucket5) }
_json($result)
raise "expect failed" unless ($result == [OpenStruct.new(bucket1: 7.0, bucket2: 15.0, bucket3: 30.0, bucket4: 35.0, bucket5: 50.0)])
