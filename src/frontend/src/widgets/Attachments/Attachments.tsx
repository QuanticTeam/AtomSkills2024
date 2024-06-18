import { SearchOutlined } from '@ant-design/icons'
import { useQuery } from '@tanstack/react-query'
import {
  Button,
  Input,
  InputRef,
  Space,
  Table,
  TableColumnType,
  TableProps,
  Typography,
} from 'antd'
import { FilterDropdownProps } from 'antd/es/table/interface'
import { useEffect, useRef, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { Attachment, AttachmentsApi } from '~/entities'
import { FilterType, SortAndFilterRequest, getSortAndFilterRequestPayload } from '~/shared/api'

export interface AttachmentsProps {
  fileKeys: string[]
}

type DataIndex = keyof Attachment

export function Attachments({ fileKeys }: AttachmentsProps) {
  const { t } = useTranslation(Attachments.name)

  const [searchText, setSearchText] = useState('')
  const [searchedColumn, setSearchedColumn] = useState('')
  const searchInput = useRef<InputRef>(null)

  const handleSearch = (
    selectedKeys: string[],
    confirm: FilterDropdownProps['confirm'],
    dataIndex: DataIndex,
  ) => {
    confirm()
    setSearchText(selectedKeys[0])
    setSearchedColumn(dataIndex)
  }

  const handleReset = (clearFilters: () => void, confirm: () => void) => {
    clearFilters()
    setSearchText('')
    setSearchedColumn('')
    confirm()
  }

  const getColumnSearchProps = (dataIndex: DataIndex): TableColumnType<Attachment> => ({
    filterDropdown: ({ setSelectedKeys, selectedKeys, confirm, clearFilters, close }) => (
      <div
        style={{ padding: 8 }}
        onKeyDown={e => e.stopPropagation()}
      >
        <Input
          ref={searchInput}
          placeholder={`Search ${dataIndex}`}
          value={selectedKeys[0]}
          onChange={e => setSelectedKeys(e.target.value ? [e.target.value] : [])}
          onPressEnter={() => handleSearch(selectedKeys as string[], confirm, dataIndex)}
          style={{ marginBottom: 8, display: 'block' }}
        />
        <Space>
          <Button
            type="primary"
            onClick={() => handleSearch(selectedKeys as string[], confirm, dataIndex)}
            icon={<SearchOutlined />}
            size="small"
            style={{ width: 90 }}
          >
            Search
          </Button>
          <Button
            onClick={() => clearFilters && handleReset(clearFilters, confirm)}
            size="small"
            style={{ width: 90 }}
          >
            Reset
          </Button>
        </Space>
      </div>
    ),
    filterIcon: (filtered: boolean) => (
      <SearchOutlined style={{ color: filtered ? '#1677ff' : undefined }} />
    ),
    // onFilter: (value, record) =>
    //   record[dataIndex]
    //     .toString()
    //     .toLowerCase()
    //     .includes((value as string).toLowerCase()),
    onFilterDropdownOpenChange: visible => {
      if (visible) {
        setTimeout(() => searchInput.current?.select(), 100)
      }
    },
    // render: text =>
    //   searchedColumn === dataIndex ? (
    //     <SearchHighlighter
    //       searchWords={[searchText]}
    //       textToHighlight={text}
    //     />
    //   ) : (
    //     text
    //   ),
  })

  const columns: TableProps<Attachment>['columns'] = [
    {
      title: 'Имя',
      dataIndex: 'originalNme',
      key: 'originalNme',
    },
    {
      title: t('colTitle'),
      dataIndex: 'title',
      key: 'title',
    },
    {
      title: '',
      key: 'action',
      render: (_, record) => (
        <Space size="middle">
          <Typography.Link href={`/api/File/Download/${record.code}`}>Скачать</Typography.Link>
        </Space>
      ),
    },
  ]

  const [sortAndFilterPayload, setSortAndFilterPayload] = useState<SortAndFilterRequest>(
    getSortAndFilterRequestPayload(),
  )

  const { data, error, isPending, refetch } = useQuery({
    queryKey: ['attachments'],
    queryFn: () => AttachmentsApi.getByIds(fileKeys),
  })

  useEffect(() => {
    refetch()
  }, [refetch, sortAndFilterPayload])

  const onTableChange: TableProps<Attachment>['onChange'] = (
    pagination,
    filters,
    sorter,
    extra,
  ) => {
    switch (extra.action) {
      case 'sort': {
        if (!Array.isArray(sorter)) {
          console.log(sorter)
          setSortAndFilterPayload(prev => ({
            ...prev,
            orderBy: sorter.order ? (sorter.columnKey as string) : '',
            descending: sorter.order === ('descend' as string),
          }))
        }

        break
      }

      case 'filter': {
        setSortAndFilterPayload(prev => ({
          ...prev,
          filters: Object.entries(filters)
            .filter(x => x[1] && x[1].length)
            .map(([columnName, values]) => ({
              columnName,
              values: (values as string[]) ?? [],
              filterType: FilterType.Contains,
            })),
        }))

        break
      }
    }

    console.log('params', { pagination, filters, sorter, extra })
  }

  return (
    <Table
      bordered
      columns={columns}
      dataSource={data}
      pagination={false}
      onChange={onTableChange}
      scroll={{
        x: 0,
        y: 500,
      }}
    />
  )
}
